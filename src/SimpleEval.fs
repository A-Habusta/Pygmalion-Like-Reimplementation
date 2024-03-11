module PygmalionReimplementation.SimpleEval

open PygmalionReimplementation.Utils

type UnderlyingNumberDataType = int
type IconInstructionParameter = UnderlyingNumberDataType option

type UnaryOperation =
    { Name : string
      Op : (UnderlyingNumberDataType -> UnderlyingNumberDataType) }
type BinaryOperation =
    { Name : string
      Op : (UnderlyingNumberDataType -> UnderlyingNumberDataType -> UnderlyingNumberDataType) }

exception TrapException

let evalUnaryOperation (operation : UnaryOperation) rawOperand =
    match rawOperand with
    | Some operand -> operation.Op operand
    | None -> raise TrapException

let evalBinaryOperation (operation : BinaryOperation) rawOperand1 rawOperand2 =
    match (rawOperand1, rawOperand2) with
    | (None, _) -> raise TrapException
    | (_, None) -> raise TrapException
    | (Some operand1, Some operand2) -> operation.Op operand1 operand2