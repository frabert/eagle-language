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

    let (=>) a (t2, b) =
        match a with
        | Success((t1, res), state, pos) ->
            t1 |> should equal t2
            res |> printExpression |> should equal b
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
        parseExpression "true" => (BaseType "bool", "true")
        parseExpression "false" => (BaseType "bool", "false")
        parseExpression "true " => (BaseType "bool", "true")
        parseExpression "false " => (BaseType "bool", "false")

        parseExpression "treu" |> isInvalid
        parseExpression "flase" |> isInvalid

    [<Fact>]
    let ``Integer parsing``() =
        parseExpression "12uy" => (BaseType "uint8", "12uy")
        parseExpression "-2y" => (BaseType "int8", "-(2y)")
        parseExpression "-2uy" => (BaseType "int16", "-(2uy)")
        parseExpression "256us" => (BaseType "uint16", "256us")
        parseExpression "-255s" => (BaseType "int16", "-(255s)")
        parseExpression "65536u" => (BaseType "uint32", "65536ul")
        parseExpression "-65535" => (BaseType "int32", "-(65535l)")
        parseExpression "4294967296UL" => (BaseType "uint64", "4294967296UL")
        parseExpression "-4294967295L" => (BaseType "int64", "-(4294967295L)")
        
    [<Fact>]
    let ``Float parsing``() =
        parseExpression "0.1f" => (BaseType "float32", "0.100000f")
        parseExpression "0.1" => (BaseType "float64", "0.100000d")
        parseExpression "5e+39" => (BaseType "float64", "5000000000000000000000000000000000000000.000000d")

    [<Fact>]
    let ``String parsing``() =
        parseExpression "\"hello\"" => (BaseType "string", "\"hello\"")
        parseExpression "\"hel\\u23fflo\"" => (BaseType "string", "\"hel\u23fflo\"")

    [<Fact>]
    let ``Variable parsing``() =
        parseExpression "foo" => (BaseType "string", "foo")
        parseExpression "bar" => (BaseType "int64", "bar")
        parseExpression "foobar" => (BaseType "bool", "foobar")
        parseExpression "fubar" |> isInvalid

    [<Fact>]
    let ``ITE Parsing`` () =
        parseExpression "if true then 3 else 4" =>
            (BaseType "int32", "if (true) then (3l) else (4l)")
        parseExpression "if false then \"hello\" else \"world\"" =>
            (BaseType "string", "if (false) then (\"hello\") else (\"world\")")
        parseExpression "if foobar then \"hello\" else \"world\"" =>
            (BaseType "string", "if (foobar) then (\"hello\") else (\"world\")")

    [<Fact>]
    let ``Function call parsing`` () =
        parseExpression "boh(3, 4 ,5  )" => (BaseType "uint8", "boh((3l),(4l),(5l))")
        parseExpression "boh(3,4,true)" |> isInvalid
        parseExpression "boh(3,4)" |> isInvalid
        parseExpression "bho(3,4,5)" |> isInvalid

    [<Fact>]
    let ``Operator expression parsing`` () =
        parseExpression "3+4" => (BaseType "int32", "(3l)+(4l)")
        parseExpression "bar+3" => (BaseType "int64", "(bar)+(3l)")
        parseExpression "1+boh(3+3,4,5)" => (BaseType "int32", "(1l)+(boh(((3l)+(3l)),(4l),(5l)))")
        parseExpression "3-4" => (BaseType "int32", "(3l)-(4l)")
        parseExpression "3*4" => (BaseType "int32", "(3l)*(4l)")
        parseExpression "3/4" => (BaseType "int32", "(3l)/(4l)")
        parseExpression "3+4*3+4" => (BaseType "int32", "(3l)+(((4l)*(3l))+(4l))")
        parseExpression "3+3 < 4+4" => (BaseType "bool", "((3l)+(3l))<((4l)+(4l))")
        parseExpression "!(3+3 < 4+4)" => (BaseType "bool", "!(((3l)+(3l))<((4l)+(4l)))")

        parseExpression "3+true" |> isInvalid
        parseExpression "3>true" |> isInvalid
        parseExpression "!(3+4)" |> isInvalid