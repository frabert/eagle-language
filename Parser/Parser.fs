namespace Eagle

open Types
open SyntaxElements
open FParsec
open FParsec.CharParsers

module Parser =
    type VariableScope = {
        parent:VariableScope option;
        variables: Map<string, EagleType>
    }

    type ParserGlobalState = {
        types: Map<string, EagleType>;
        globals: Map<string, EagleType>;
        functions: Map<string, (EagleType * EagleType list)>;
        procedures: Map<string, (EagleType * EagleType list)>
    }

    type ExpressionParser = Parser<(EagleType * Expression), (ParserGlobalState * VariableScope)>
    type StatementParser = Parser<Statement, (ParserGlobalState * VariableScope)>

    let searchVariable (state : ParserGlobalState) (scope : VariableScope) (name : string) : EagleType option =
        let rec searchScope s =
            match s.variables.TryFind(name) with
            | Some t -> Some t
            | _ -> Option.bind searchScope s.parent
        match searchScope scope with
            | Some t -> Some t
            | _ -> state.globals.TryFind(name)

    let addVariable scope name vartype = { scope with variables = scope.variables.Add(name, vartype) }

    let emptyScope = { parent = None; variables = Map.empty }
    let emptyState = { types = Map.empty; globals = Map.empty; functions = Map.empty; procedures = Map.empty }

    let pushScope parentScope = {
        parent = Some parentScope;
        variables = Map.empty
    }

    let popScope scope =
        match scope.parent with
        | Some s -> s
        | None -> emptyScope

    let pushScopeStream = updateUserState (fun (state, scope) -> (state, pushScope scope))
    let popScopeStream = updateUserState (fun (state, scope) -> (state, popScope scope))

    let addType state name t = { state with types = state.types.Add(name, t) }
    let addGlobal state name t = { state with globals = state.globals.Add(name, t) }
    let addFunction state name t = { state with functions = state.functions.Add(name, t) }
    let addProcedure state name t = { state with procedures = state.procedures.Add(name, t) }
    let searchType state name =
        match name with
        | "int8"
        | "int16"
        | "int32"
        | "int64"
        | "uint8"
        | "uint16"
        | "uint32"
        | "uint64"
        | "float32"
        | "float64"
        | "bool"
        | "string" -> Some (BaseType name)
        | _ -> state.types.TryFind(name)

    let ws = spaces
    let pstr_ws s = skipString s .>> ws
    let ident = identifier (IdentifierOptions ( )) .>> ws

    let expr, exprRef = createParserForwardedToRef<(EagleType * Expression), (ParserGlobalState * VariableScope)>()
    let stat, statRef = createParserForwardedToRef<Statement, (ParserGlobalState * VariableScope)>()

    let pNumberExpr : ExpressionParser =
        let numberFormat =
            NumberLiteralOptions.AllowMinusSign
            ||| NumberLiteralOptions.AllowFraction
            ||| NumberLiteralOptions.AllowExponent
            ||| NumberLiteralOptions.AllowSuffix
        numberLiteral numberFormat "number"
        >>= fun nl ->
                if nl.IsInteger then
                    let varType =
                        match nl.SuffixChar1, nl.SuffixChar2, nl.SuffixChar3 with
                        | 'u', 'y', EOS -> preturn (BaseType "uint8", uint8 >> UInt8Literal)
                        | 'y', EOS, EOS -> preturn (BaseType "int8", int8 >> Int8Literal)
                        | 'u', 's', EOS -> preturn (BaseType "uint16", uint16 >> UInt16Literal)
                        | 's', EOS, EOS -> preturn (BaseType "int16", int16 >> Int16Literal)
                        | 'u', 'l', EOS -> preturn (BaseType "uint32", uint32 >> UInt32Literal)
                        | 'l', EOS, EOS -> preturn (BaseType "int32", int32 >> Int32Literal)
                        | 'U', 'L', EOS -> preturn (BaseType "uint64", uint64 >> UInt64Literal)
                        | 'L', EOS, EOS -> preturn (BaseType "int64", int64 >> Int64Literal)
                        | EOS, EOS, EOS -> preturn (BaseType "int32", int32 >> Int32Literal)
                        | 'u', EOS, EOS -> preturn (BaseType "uint32", uint32 >> UInt32Literal)
                        | _, _, _ -> failFatally "Invalid integer suffix"

                    varType >>= fun (t, f) -> preturn (t, f nl.String)
                else
                    let varType =
                        match nl.SuffixChar1, nl.SuffixChar2 with
                        | 'f', EOS -> preturn (BaseType "float32", float32 >> Float32Literal)
                        | 'd', EOS -> preturn (BaseType "float64", float >> Float64Literal)
                        | EOS, EOS -> preturn (BaseType "float64", float >> Float64Literal)
                        | _, _ -> failFatally "Invalid floating point suffix"

                    varType >>= fun (t, f) -> preturn (t, f nl.String)

    let pBoolExpr : ExpressionParser =
        choice [
            stringReturn "true" (BaseType "bool", BooleanLiteral true)
            stringReturn "false" (BaseType "bool", BooleanLiteral false)
        ]

    let pStringExpr : ExpressionParser =
        let escape =  anyOf "\"\\/bfnrt"
                  |>> function
                      | 'b' -> "\b"
                      | 'f' -> "\u000C"
                      | 'n' -> "\n"
                      | 'r' -> "\r"
                      | 't' -> "\t"
                      | c   -> string c // every other char is mapped to
        let unicodeEscape =
            /// converts a hex char ([0-9a-fA-F]) to its integer number (0-15)
            let hex2int c = (int c &&& 15) + (int c >>> 6)*9

            pstring "u" >>. pipe4 hex hex hex hex (fun h3 h2 h1 h0 ->
                (hex2int h3)*4096 + (hex2int h2)*256 + (hex2int h1)*16 + hex2int h0
                |> char |> string
            )

        let escapedCharSnippet = pstring "\\" >>. (escape <|> unicodeEscape)
        let normalCharSnippet  = manySatisfy (fun c -> c <> '"' && c <> '\\')

        between (pstring "\"") (pstring "\"")
            (stringsSepBy normalCharSnippet escapedCharSnippet)
            >>= fun s -> preturn (BaseType "string", StringLiteral s)

    let pVariableExpr : ExpressionParser =
        ident >>= fun name ->
            fun stream ->
                let (state, scope) = stream.UserState
                match searchVariable state scope name with
                | Some t -> Reply((t, Variable name))
                | None -> Reply(FatalError, messageError ("Variable not found: " + name))

    // Parses an expression of type t. If the expression is not of the correct type,
    // a (non?) fatal error is generated with message specified by error
    let pExpressionType t error fatal =
        (expr <?> (sprintf "expression of type %A" t)) >>=
        fun (exprType, expression) ->
            fun stream ->
                if exprType <> t && not (canPromote exprType t) then
                    Reply((if fatal then FatalError else Error), messageError (error + sprintf " Expected type: %A, found: %A" t exprType))
                else
                    Reply(expression)

    let pITEExpr : ExpressionParser =
        let pCondition = pExpressionType (BaseType "bool") "If condition must be boolean." false
        tuple3 (pstr_ws "if" >>. pCondition) (pstr_ws "then" >>. expr) (pstr_ws "else" >>. expr) >>=
        fun (cond, (ifType, ifBranch), (elseType, elseBranch)) ->
            fun stream ->
                if ifType <> elseType then
                    Reply(FatalError, messageError "The types of the branches of an IfThenElse expression must match.")
                else
                    Reply((ifType, ITE (cond, ifBranch, elseBranch)))

    let pCallExpr : ExpressionParser =
        let rec args = function
            | [] -> ws >>. preturn []
            | [x] -> pExpressionType x "Wrong argument type." true >>= fun x -> preturn [x]
            | x::xs -> (pExpressionType x "Wrong argument type." true) .>> (pstr_ws "," <??> "Too few arguments.") .>>. args xs >>= fun (x', xs') -> preturn (x'::xs')
        attempt (ident .>> pstr_ws "(") >>= fun name ->
            fun stream ->
                let (state, scope) = stream.UserState
                let funnelReply t name (reply : Reply<Expression list>)=
                    if reply.Status = ReplyStatus.Ok then
                        Reply ((t, Call("", name, reply.Result)))
                    else
                        Reply(reply.Status, reply.Error)
                match state.functions.TryFind(name), state.procedures.TryFind(name) with
                | Some (t, f), None ->
                    let argsReply = args f stream
                    funnelReply t name argsReply
                | None, Some (t, p) ->
                    let argsReply = args p stream
                    funnelReply t name argsReply
                | None, None -> Reply (FatalError, messageError ("Function or procedure not found: " + name))
                | Some _, Some _ -> Reply(FatalError, messageError ("Ambiguous call between procedure and function: " + name))
        .>> pstr_ws ")"

    type MaybeExpression =
        | Expr of EagleType * Expression
        | Error of Position * string
    
    let baseExpr =
        choice [
            between (pstr_ws "(") (pstr_ws ")") expr
            pITEExpr
            pBoolExpr
            pNumberExpr
            pStringExpr
            pCallExpr
            pVariableExpr
        ] .>> ws

    let pOperatorExpr : ExpressionParser =
        let opp = new OperatorPrecedenceParser<MaybeExpression, Position, _>()
        let mexprParser = baseExpr >>= fun (t, ex) -> preturn (Expr (t, ex))
        let opWrapper f =
            fun pos mexpr1 mexpr2 ->
                match mexpr1, mexpr2 with
                | Expr (type1, expr1), Expr(type2, expr2) ->
                    f pos (type1, expr1) (type2, expr2)
                | Error _ as a, Error _ -> a
                | Error _ as a, _ -> a
                | _, (Error _ as a) -> a

        let arithmeticOperator op =
            opWrapper (fun pos (type1, expr1) (type2, expr2) ->
                if not (isNumericType type1) || not (isNumericType type2) then
                    Error (pos, "Cannot operate on non-numerical values.")
                elif type1 <> type2 && not (canPromote type1 type2) && not (canPromote type2 type1) then
                    Error (pos, "Cannot find a suitable container type for the operation.")
                else
                    let container = containerType type1 type2
                    Expr (container, Binary (op, expr1, expr2)))
        
        let comparisonOperator op =
            opWrapper (fun pos (type1, expr1) (type2, expr2) ->
                if not (isNumericType type1) || not (isNumericType type2) then
                    Error (pos, "Cannot operate on non-numerical values.")
                else
                    Expr (BaseType "bool", Binary (op, expr1, expr2)))

        let booleanOperator op =
            opWrapper (fun pos (type1, expr1) (type2, expr2) ->
                if type1 <> BaseType "bool" || type2 <> BaseType "bool" then
                    Error (pos, "Cannot operate on non-boolean values.")
                else
                    Expr (BaseType "bool", Binary (op, expr1, expr2)))

        opp.AddOperator(PrefixOperator("-", getPosition .>> ws, 7, true, (), fun pos mexpr ->
            match mexpr with
            | Expr (t, exp) ->
                if not (isNumericType t) then
                    Error (pos, "Cannot operate on non-numeric values.")
                else
                    let expr = Unary (Invert, exp)
                    if isUnsigned t then
                        Expr (unsignedToSigned t, expr)
                    else
                        Expr (t, expr)
            | Error _ as a -> a
        ))

        opp.AddOperator(PrefixOperator("!", getPosition .>> ws, 7, true, (), fun pos mexpr ->
            match mexpr with
            | Expr (t, exp) ->
                if t <> BaseType "bool" then
                    Error (pos, "Cannot operate on non-boolean values.")
                else
                    Expr (t, Unary (Negate, exp))
            | Error _ as a -> a
        ))
        opp.AddOperator(InfixOperator("*", getPosition .>> ws, 6, Associativity.Right, (), arithmeticOperator Mul))
        opp.AddOperator(InfixOperator("/", getPosition .>> ws, 6, Associativity.Right, (), arithmeticOperator Div))
        opp.AddOperator(InfixOperator("%", getPosition .>> ws, 6, Associativity.Right, (), arithmeticOperator Mod))
        opp.AddOperator(InfixOperator("+", getPosition .>> ws, 5, Associativity.Right, (), arithmeticOperator Add))
        opp.AddOperator(InfixOperator("-", getPosition .>> ws, 5, Associativity.Right, (), arithmeticOperator Sub))
        opp.AddOperator(InfixOperator(">", getPosition .>> ws, 4, Associativity.Right, (), comparisonOperator Gt))
        opp.AddOperator(InfixOperator(">=", getPosition .>> ws, 4, Associativity.Right, (), comparisonOperator Ge))
        opp.AddOperator(InfixOperator("<", getPosition .>> ws, 4, Associativity.Right, (), comparisonOperator Lt))
        opp.AddOperator(InfixOperator("<=", getPosition .>> ws, 4, Associativity.Right, (), comparisonOperator Le))
        opp.AddOperator(InfixOperator("==", getPosition .>> ws, 3, Associativity.Right, (), comparisonOperator Eq))
        opp.AddOperator(InfixOperator("!=", getPosition .>> ws, 3, Associativity.Right, (), comparisonOperator NotEq))
        opp.AddOperator(InfixOperator("&&", getPosition .>> ws, 2, Associativity.Right, (), comparisonOperator And))
        opp.AddOperator(InfixOperator("||", getPosition .>> ws, 1, Associativity.Right, (), comparisonOperator Or))
        opp.TermParser <- mexprParser
        opp.ExpressionParser >>= fun x ->
            match x with
            | Expr (t, e) -> preturn (t, e)
            | Error (p, s) ->
                fun stream ->
                    stream.Skip(p.Index - stream.Position.Index)
                    Reply(FatalError, messageError s)

    let pVariableDecl : StatementParser =
        ((pstr_ws "var" >>. ident .>> pstr_ws "=") .>>. expr)
        >>= fun (name, (t, expr)) ->
            fun stream ->
                let (state, scope) = stream.UserState
                let newState = (state, addVariable scope name t)
                stream.UserState <- newState
                Reply(VarAssignment (name, expr))

    let pWhile : StatementParser =
        let pCondition = pExpressionType (BaseType "bool") "While condition must be boolean." false
        pstr_ws "while" >>. (between (pstr_ws "(") (pstr_ws ")") pCondition) .>> pushScopeStream .>>. stat
        >>= fun (condition, body) -> preturn (While (condition, body)) .>> popScopeStream

    let pBlock : StatementParser =
        pstr_ws "{" >>. pushScopeStream >>. many stat .>> pstr_ws "}" .>> popScopeStream
        >>= (Block >> preturn)

    let pVariableAssign : StatementParser =
        (ident .>> pstr_ws "=" >>= fun name ->
            fun stream ->
                let (state, scope) = stream.UserState
                match searchVariable state scope name with
                | Some v -> Reply((name, v))
                | None -> Reply(FatalError, messageError ("Assignment to undefined variable: " + name)))
        >>= fun (name, t) ->
            (pExpressionType t "Invalid assignment type." true)
            >>= fun expr -> preturn (Assignment (name, expr))

    do
        exprRef := (pOperatorExpr .>> ws) <?> "expression"

        statRef :=
            (choice [
                pVariableDecl
                pWhile
                pBlock
                pVariableAssign
            ] .>> ws) <?> "statement"
