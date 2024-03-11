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
    list
    |> List.mapi (fun i x -> (i, x))
    |> List.filter (fun (i, _) -> i <> index)
    |> List.map snd

let listReplaceIndex index value list =
    list
    |> List.mapi (fun i x -> if i = index then value else x)

let listLast list =
    let rec listLast' list acc =
        match list with
        | [] -> acc
        | x :: xs -> listLast' xs x
    listLast' list (List.head list)

let isNumber (text : string) =
    match Int32.TryParse text with
    | true, _ -> true
    | _ -> false

let isText (text : string) =
    String.length text > 0


let cons x y = x :: y