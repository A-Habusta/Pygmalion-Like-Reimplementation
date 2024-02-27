module PygmalionReimplementation.Utils

open System

let FalseValue = 0
let TrueValue = 1

let boolToInt b =
    if b then TrueValue else FalseValue

let intToBool i =
    i <> FalseValue

let binaryFuncResultConverter convertRes f a b =
    let res = f a b
    convertRes res

let binaryFuncInputConverter convertA convertB f a b =
    let a' = convertA a
    let b' = convertB b
    f a' b'

let listRemoveIndex index list =
    let rec listRemoveIndex' index list acc =
        match list with
        | [] -> acc
        | x :: xs when index = 0 -> acc @ xs
        | x :: xs -> listRemoveIndex' (index - 1) xs (acc @ [x])
    listRemoveIndex' index list []

let listReplaceIndex index value list =
    let rec listReplaceIndex' index value list acc =
        match list with
        | [] -> acc
        | x :: xs when index = 0 -> acc @ [value] @ xs
        | x :: xs -> listReplaceIndex' (index - 1) value xs (acc @ [x])
    listReplaceIndex' index value list []

let isNumber (text : string) =
    match Int32.TryParse text with
    | true, _ -> true
    | _ -> false

let isText (text : string) =
    String.length text > 0