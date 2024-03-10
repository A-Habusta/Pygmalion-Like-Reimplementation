module PygmalionReimplementation.SimpleEval

open PygmalionReimplementation.Utils

type UnderlyingNumberDataType = int
type IconInstructionParameter = UnderlyingNumberDataType option

exception TrapException

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
    match (operator, operand) with
    | ("!", Some operand) -> if operand = FalseValue then TrueValue else FalseValue
    | ("-", Some operand) -> -operand
    | ("+", Some operand) -> operand
    | (_, Some _) -> failwith "Unknown unary operator"
    | _ -> raise TrapException

let evalBinary operator rawOperand1 rawOperand2 =
    match (operator, rawOperand1, rawOperand2) with
    | (_, None, _) -> raise TrapException
    | (_, _, None) -> raise TrapException
    | (operator, Some operand1, Some operand2) ->
        getCorrectBinaryOperation operator operand1 operand2