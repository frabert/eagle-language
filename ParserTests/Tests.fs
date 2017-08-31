namespace Eagle
open Xunit
open FsUnit.Xunit

open FParsec.CharParsers
open Parser
open SyntaxElements
open Types
    
module ``Expression parsing`` =
    let (==>) a b =
        match a with
        | Success(res, state, pos) ->
            res |> should equal b
        | Failure (msg, error, state) -> failwith msg

    let (=/=>) a b =
        match a with
        | Success(res, state, pos) ->
            res |> should not' (equal res)
        | Failure _ -> failwith "Parsing failed"

    let isInvalid a =
        match a with
        | Success _ -> failwith "Parsing succeded, expected failure"
        | Failure _ -> ()
    
    let parseExpression s =
        let variables = Map [("foo", BaseType "string")
                             ("bar", BaseType "int64")
                             ("foobar", BaseType "bool")]
        let functions = Map [("boh", (BaseType "uint8", [BaseType "int32"; BaseType "int32"; BaseType "int32"]))]
        let state = {emptyState with functions = functions}
        runParserOnString pOperatorExpr (state, {parent = None; variables = variables}) "" s

    [<Fact>]
    let ``Boolean parsing`` () =
        parseExpression "true"    ==> ((BaseType "bool"), (BooleanLiteral true))
        parseExpression "false"   ==> ((BaseType "bool"), (BooleanLiteral false))
        parseExpression "true "   ==> ((BaseType "bool"), (BooleanLiteral true))
        parseExpression "false "  ==> ((BaseType "bool"), (BooleanLiteral false))

        parseExpression "treu" |> isInvalid
        parseExpression "flase" |> isInvalid

    [<Fact>]
    let ``Integer parsing``() =
        parseExpression "12uy" ==> ((BaseType "uint8"), (UInt8Literal 12uy))
        parseExpression "-2y" ==> (BaseType "int8", Unary (Invert, Int8Literal 2y))
        parseExpression "-2uy" ==> (BaseType "int16", Unary (Invert, UInt8Literal 2uy))
        parseExpression "256us" ==> ((BaseType "uint16"), (UInt16Literal 256us))
        parseExpression "-255s" ==> (BaseType "int16", Unary (Invert, Int16Literal 255s))
        parseExpression "65536u" ==> ((BaseType "uint32"), (UInt32Literal 65536u))
        parseExpression "-65535" ==> (BaseType "int32", Unary (Invert, Int32Literal 65535))
        parseExpression "4294967296UL" ==> ((BaseType "uint64"), (UInt64Literal 4294967296UL))
        parseExpression "-4294967295L" ==> (BaseType "int64", Unary (Invert, Int64Literal 4294967295L))
        
    [<Fact>]
    let ``Float parsing``() =
        parseExpression "0.1f" ==> ((BaseType "float32"), (Float32Literal 0.1f))
        parseExpression "0.1" ==> ((BaseType "float64"), (Float64Literal 0.1))
        parseExpression "5e+39" ==> ((BaseType "float64"), (Float64Literal 5e+39))

    [<Fact>]
    let ``String parsing``() =
        parseExpression "\"hello\"" ==> (BaseType "string", StringLiteral "hello")
        parseExpression "\"hel\\u23fflo\"" ==> (BaseType "string", StringLiteral "hel\u23fflo")

    [<Fact>]
    let ``Variable parsing``() =
        parseExpression "foo" ==> (BaseType "string", Variable "foo")
        parseExpression "bar" ==> (BaseType "int64", Variable "bar")
        parseExpression "foobar" ==> (BaseType "bool", Variable "foobar")
        parseExpression "fubar" |> isInvalid

    [<Fact>]
    let ``ITE Parsing`` () =
        parseExpression "if true then 3 else 4" ==>
            (BaseType "int32", ITE(BooleanLiteral true,
                Int32Literal 3,
                Int32Literal 4))
        parseExpression "if false then \"hello\" else \"world\"" ==>
            (BaseType "string", ITE(BooleanLiteral false,
                StringLiteral "hello",
                StringLiteral "world"))
        parseExpression "if foobar then \"hello\" else \"world\"" ==>
            (BaseType "string", ITE(Variable "foobar",
                StringLiteral "hello",
                StringLiteral "world"))

    [<Fact>]
    let ``Function call parsing`` () =
        parseExpression "boh(3, 4 ,5  )" ==> (BaseType "uint8", Call("", "boh", [Int32Literal 3; Int32Literal 4; Int32Literal 5]))
        parseExpression "boh(3,4,true)" |> isInvalid
        parseExpression "boh(3,4)" |> isInvalid
        parseExpression "bho(3,4,5)" |> isInvalid

    [<Fact>]
    let ``Operator expression parsing`` () =
        parseExpression "3+4" ==> (BaseType "int32", Binary(Add, Int32Literal 3, Int32Literal 4))
        parseExpression "bar+3" ==> (BaseType "int64", Binary(Add, Variable "bar", Int32Literal 3))
        parseExpression "1+boh(3+3,4,5)" ==> (BaseType "int32", Binary(Add, Int32Literal 1, Call("", "boh", [Binary(Add, Int32Literal 3, Int32Literal 3); Int32Literal 4; Int32Literal 5])))
        parseExpression "3-4" ==> (BaseType "int32", Binary(Sub, Int32Literal 3, Int32Literal 4))
        parseExpression "3*4" ==> (BaseType "int32", Binary(Mul, Int32Literal 3, Int32Literal 4))
        parseExpression "3/4" ==> (BaseType "int32", Binary(Div, Int32Literal 3, Int32Literal 4))
        parseExpression "3+4*3+4" ==> (BaseType "int32", Binary(Add, Int32Literal 3, Binary(Add, Binary(Mul, Int32Literal 4, Int32Literal 3), Int32Literal 4)))

        parseExpression "3+true" |> isInvalid
        parseExpression "3>true" |> isInvalid
        parseExpression "!(3+4)" |> isInvalid