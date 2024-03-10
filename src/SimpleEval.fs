module PygmalionReimplementation.SimpleEval

open PygmalionReimplementation.Utils

let getCorrectBoolBinaryOperation operatorString =
    let inputToBool = binaryFuncInputConverter intToBool intToBool
    let matchFunc =
        function
        | "&&" -> (&&)
        | "||" -> (||)
        | _ -> failwith "Unknown binary operator"

    inputToBool (matchFunc operatorString)

let getCorrectCompareBinaryOperation operatorString =
    let boolToInt = binaryFuncResultConverter boolToInt
    let matchFunc =
        function
        | "<" -> (<)
        | "<=" -> (<=)
        | ">" -> (>)
        | ">=" -> (>=)
        | "=" -> (=)
        | "<>" -> (<>)
        | _ -> getCorrectBoolBinaryOperation operatorString

    boolToInt (matchFunc operatorString)

let getCorrectBinaryOperation operator =
    match operator with
    | "+" -> (+)
    | "-" -> (-)
    | "*" -> (*)
    | "/" -> (/)
    | "%" -> (%)
    | _ -> getCorrectCompareBinaryOperation operator

let evalUnary operator operand =
    match operator with
    | "!" -> if operand = FalseValue then TrueValue else FalseValue
    | "-" -> -operand
    | "+" -> operand
    | _ -> failwith "Unknown unary operator"

let evalBinary operator rawOperand1 rawOperand2 =
    getCorrectBinaryOperation operator rawOperand1 rawOperand2