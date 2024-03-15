module PygmalionReimplementation.SimpleEval

type UnderlyingNumberDataType = int
type IconInstructionParameter = UnderlyingNumberDataType option

type UnaryIconFunction = (UnderlyingNumberDataType -> UnderlyingNumberDataType)
type BinaryIconFunction = (UnderlyingNumberDataType -> UnderlyingNumberDataType -> UnderlyingNumberDataType)

let stringToUnderlyingNumberDataType (input : string) : UnderlyingNumberDataType =
    int input

let evalUnaryOperation (operation : UnaryIconFunction) operand =
    Option.map operation operand

let evalBinaryOperation (operation : BinaryIconFunction) operand1 operand2 =
    Option.map2 operation operand1 operand2