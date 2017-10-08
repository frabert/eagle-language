namespace Eagle

module SyntaxElements =
    type BinaryOp =
        | Add
        | Sub
        | Mul
        | Div
        | Mod
        | Eq
        | NotEq
        | Gt
        | Lt
        | Ge
        | Le
        | And
        | Or

    type UnaryOp =
        | Invert
        | Negate

    type Expression =
        | StringLiteral of string
        | Int8Literal of int8
        | Int16Literal of int16
        | Int32Literal of int32
        | Int64Literal of int64
        | UInt8Literal of uint8
        | UInt16Literal of uint16
        | UInt32Literal of uint32
        | UInt64Literal of uint64
        | Float32Literal of float32
        | Float64Literal of float
        | BooleanLiteral of bool
        | Binary of BinaryOp * Expression * Expression
        | Unary of UnaryOp * Expression
        | Call of string * string * (Expression list)
        | ITE of Expression * Expression * Expression
        | Variable of string
        | ArrayAccess of Expression * Expression

    let rec printExpression = function
        | StringLiteral s -> sprintf "\"%s\"" s
        | Int8Literal i -> sprintf "%iy" i
        | Int16Literal i -> sprintf "%is" i 
        | Int32Literal i -> sprintf "%il" i 
        | Int64Literal i -> sprintf "%iL" i
        | UInt8Literal i -> sprintf "%iuy" i
        | UInt16Literal i -> sprintf "%ius" i 
        | UInt32Literal i -> sprintf "%iul" i 
        | UInt64Literal i -> sprintf "%iUL" i
        | Float32Literal f -> sprintf "%ff" f
        | Float64Literal f -> sprintf "%fd" f
        | BooleanLiteral true -> sprintf "true"
        | BooleanLiteral false -> sprintf "false"
        | Binary (op, l, r) ->
            let operator =
                match op with
                | Add -> "+"
                | Sub -> "-"
                | Mul -> "*"
                | Div -> "/"
                | Mod -> "%"
                | Eq -> "=="
                | NotEq -> "!="
                | Gt -> ">"
                | Lt -> "<"
                | Ge -> ">="
                | Le -> "<="
                | And -> "&&"
                | Or -> "||"
            sprintf "(%s)%s(%s)" (printExpression l) operator (printExpression r)
        | Unary (op, e) ->
            let operator =
                match op with
                | Negate -> "!"
                | Invert -> "-"
            sprintf "%s(%s)" operator (printExpression e)
        | Call (_, s, x::xs) -> ((sprintf "(%s)" (printExpression x)) + (List.fold (fun x -> printExpression >> sprintf "%s,(%s)" x) "" xs)) |> sprintf "%s(%s)" s
        | ITE (c, i, e) -> sprintf "if (%s) then (%s) else (%s)" (printExpression c) (printExpression i) (printExpression e)
        | Variable s -> s

    type Statement =
        | Skip
        | VarAssignment of string * Expression
        | Assignment of string * Expression
        | ArrayAssignment of Expression * Expression * Expression
        | If of Expression * Statement
        | IfElse of Expression * Statement * Statement
        | While of Expression * Statement
        | Block of Statement list
        | Return of Expression
        | Assert of Expression
        | MessageAssert of Expression * Expression

    