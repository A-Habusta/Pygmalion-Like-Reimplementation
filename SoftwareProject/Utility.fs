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