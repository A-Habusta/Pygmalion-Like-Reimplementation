module PygmalionReimplementation.SimpleEval

type UnderlyingNumberDataType = int
type IconOperationParameter = UnderlyingNumberDataType option

type UnaryIconFunction = (UnderlyingNumberDataType -> UnderlyingNumberDataType)
type BinaryIconFunction = (UnderlyingNumberDataType -> UnderlyingNumberDataType -> UnderlyingNumberDataType)

type UnaryOperation =
    { Name : string
      Op : UnaryIconFunction }
type BinaryOperation =
    { Name : string
      Op : BinaryIconFunction }


let stringToUnderlyingNumberDataType (input : string) : UnderlyingNumberDataType =
    int input

let evalUnaryOperation (operation : UnaryOperation) operand =
    Option.map operation.Op operand

let evalBinaryOperation (operation : BinaryOperation) operand1 operand2 =
    Option.map2 operation.Op operand1 operand2