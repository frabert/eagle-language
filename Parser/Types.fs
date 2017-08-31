namespace Eagle

module Types =
    type VariantConstructor = { name: string; arguments: (EagleType list) }
    and EagleType =
        | Parameter of string
        | BaseType of string
        | Structure of (string * EagleType) list
        | Variant of VariantConstructor list
        | Tuple of EagleType list
        | Array of EagleType

    let rec specializeType (types : Map<string, EagleType>) = function
        | Parameter s when types.ContainsKey s -> types.Item s
        | Structure members -> Structure (List.map (fun (name, t) -> (name, specializeType types t)) members)
        | Variant constructors -> Variant (List.map (fun c -> { name = c.name; arguments = List.map (specializeType types) c.arguments }) constructors)
        | Tuple t -> Tuple (List.map (specializeType types) t)
        | Array t -> Array (specializeType types t)
        | t -> t

    let isBaseType name =
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
            | "string" -> true
            | _ -> false

    let isNumericType t =
        match t with
            | BaseType "int8"
            | BaseType "int16"
            | BaseType "int32"
            | BaseType "int64"
            | BaseType "uint8"
            | BaseType "uint16"
            | BaseType "uint32"
            | BaseType "uint64"
            | BaseType "float32"
            | BaseType "float64" -> true
            | _ -> false

    let canPromote t1 t2 =
        match t1, t2 with
        | BaseType "int8", BaseType "int16"
        | BaseType "int8", BaseType "int32"
        | BaseType "int8", BaseType "int64"
        | BaseType "int16", BaseType "int32"
        | BaseType "int16", BaseType "int64"
        | BaseType "int32", BaseType "int64"
        | BaseType "uint8", BaseType "int16"
        | BaseType "uint8", BaseType "int32"
        | BaseType "uint8", BaseType "int64"
        | BaseType "uint16", BaseType "int32"
        | BaseType "uint16", BaseType "int64"
        | BaseType "uint32", BaseType "int64"
        | BaseType "uint8", BaseType "uint16"
        | BaseType "uint8", BaseType "uint32"
        | BaseType "uint8", BaseType "uint64"
        | BaseType "uint16", BaseType "uint32"
        | BaseType "uint16", BaseType "uint64"
        | BaseType "uint32", BaseType "uint64" -> true
        | t, BaseType "float32" when t <> (BaseType "float32") && isNumericType t -> true
        | t, BaseType "float64" when t <> (BaseType "float64") && isNumericType t -> true
        | _ -> false

    let containerType t1 t2 =
        match t1, t2 with
        | a, b when a = b -> a
        | BaseType "int16", BaseType "int8"
        | BaseType "int8", BaseType "int16" -> BaseType "int16"
        | BaseType "int32", BaseType "int8"
        | BaseType "int8", BaseType "int32" -> BaseType "int32"
        | BaseType "int64", BaseType "int8"
        | BaseType "int8", BaseType "int64" -> BaseType "int64"
        | BaseType "int32", BaseType "int16"
        | BaseType "int16", BaseType "int32" -> BaseType "int32"
        | BaseType "int64", BaseType "int16"
        | BaseType "int16", BaseType "int64" -> BaseType "int64"
        | BaseType "int64", BaseType "int32"
        | BaseType "int32", BaseType "int64" -> BaseType "int64"
        | BaseType "int16", BaseType "uint8"
        | BaseType "uint8", BaseType "int16" -> BaseType "int16"
        | BaseType "int32", BaseType "uint8"
        | BaseType "uint8", BaseType "int32" -> BaseType "int32"
        | BaseType "int64", BaseType "uint8"
        | BaseType "uint8", BaseType "int64" -> BaseType "int64"
        | BaseType "int32", BaseType "uint16"
        | BaseType "uint16", BaseType "int32" -> BaseType "int32"
        | BaseType "int64", BaseType "uint16"
        | BaseType "uint16", BaseType "int64" -> BaseType "int64"
        | BaseType "int64", BaseType "uint32"
        | BaseType "uint32", BaseType "int64" -> BaseType "int64"
        | BaseType "uint16", BaseType "uint8"
        | BaseType "uint8", BaseType "uint16" -> BaseType "uint16"
        | BaseType "uint32", BaseType "uint8"
        | BaseType "uint8", BaseType "uint32" -> BaseType "uint32"
        | BaseType "uint64", BaseType "uint8"
        | BaseType "uint8", BaseType "uint64" -> BaseType "uint64"
        | BaseType "uint32", BaseType "uint16"
        | BaseType "uint16", BaseType "uint32" -> BaseType "uint32"
        | BaseType "uint64", BaseType "uint16"
        | BaseType "uint16", BaseType "uint64" -> BaseType "uint64"
        | BaseType "uint64", BaseType "uint32"
        | BaseType "uint32", BaseType "uint64" -> BaseType "uint64"
        | BaseType "float32", t when t <> (BaseType "float32") && isNumericType t -> BaseType "float32"
        | t, BaseType "float32" when t <> (BaseType "float32") && isNumericType t -> BaseType "float32"
        | BaseType "float64", t when t <> (BaseType "float64") && isNumericType t -> BaseType "float64"
        | _ -> failwith "Invalid type operation"

    let isUnsigned = function
    | BaseType t -> t.StartsWith("uint")
    | _ -> false

    let unsignedToSigned = function
    | BaseType t when t.StartsWith("uint") ->
        let size = int (t.TrimStart("uint".ToCharArray()))
        BaseType ("int" + (string (min (size * 2) 64)))
    | _ -> failwith "Invalid type operation"