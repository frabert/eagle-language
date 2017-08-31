namespace Eagle

module SyntaxElements =
    type BinaryOp =
        | Add
        | Sub
        | Mul
        | Div
        | Mod
        | Exp
        | Eq
        | NotEq
        | Gt
        | Lt
        | Ge
        | Le
        | And
        | Or
        | Xor

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

    