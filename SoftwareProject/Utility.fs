module SoftwareProject.Utils

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


let replaceAt (position: int) (newItem: 'a) (list: 'a list) : 'a list =
    let folder item (acc, counter) =
        let actualItem = if counter = position then newItem else item
        (actualItem :: acc, counter + 1)

    List.foldBack folder list ([], 0) |> fst