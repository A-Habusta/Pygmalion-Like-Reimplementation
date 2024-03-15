module PygmalionReimplementation.SimpleEval

type UnderlyingNumberDataType = int
type IconInstructionParameter = UnderlyingNumberDataType option

type UnaryOperation =
    { Name : string
      Op : (UnderlyingNumberDataType -> UnderlyingNumberDataType) }
type BinaryOperation =
    { Name : string
      Op : (UnderlyingNumberDataType -> UnderlyingNumberDataType -> UnderlyingNumberDataType) }

let stringToUnderlyingNumberDataType (input : string) : UnderlyingNumberDataType =
    int input

let evalUnaryOperation (operation : UnaryOperation) operand =
    Option.map operation.Op operand

let evalBinaryOperation (operation : BinaryOperation) operand1 operand2 =
    Option.map2 operation.Op operand1 operand2