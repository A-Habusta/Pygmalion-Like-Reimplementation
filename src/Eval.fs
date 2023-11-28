module PygmalionReimplementation.Eval

open PygmalionReimplementation.Icons
open PygmalionReimplementation.Utils


let evalUnary operator operand =
    match operator with
    | "!" -> if operand = FalseValue then TrueValue else FalseValue
    | "-" -> -operand
    | "+" -> operand
    | _ -> failwith "Unknown unary operator"


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

type EvalContext =
    { CustomIcons : CustomIcons
      ExecutingCustomIcon : CustomIconType
      CurrentIconID : IconID
      Parameters : Lazy<int> list }

let getIconInstructionFromID (context : EvalContext) (id : IconID) =
    context.ExecutingCustomIcon.SavedIcons[id].IconInstruction

let getContextEntryPointInstruction (context : EvalContext) =
    getIconInstructionFromID context context.ExecutingCustomIcon.EntryPointIcon

exception TrapException of EvalContext

let trap (context : EvalContext) =
    raise (TrapException context)

let createContextForCustomIcon
    (oldContext : EvalContext)
    (typeName : string)
    (parameters : Lazy<int> list) =
        let newIcon = oldContext.CustomIcons[typeName]
        { oldContext with
            ExecutingCustomIcon = newIcon
            CurrentIconID = newIcon.EntryPointIcon
            Parameters = parameters }

let rec eval (context : EvalContext) (instruction : IconInstruction) =
    let boundChildrenEval = (instructionParamEval context)
    match instruction with
    | TopLevelTrap -> trap context
    | Unary(operator, operand) -> evalUnary operator (boundChildrenEval operand)
    | Binary(operator, leftOperand, rightOperand) ->
        let operation = getCorrectBinaryOperation operator
        operation (boundChildrenEval leftOperand) (boundChildrenEval rightOperand)
    | If(cond, trueBranch, falseBranch) ->
        let res = boundChildrenEval cond
        if res = FalseValue then boundChildrenEval falseBranch
        else boundChildrenEval trueBranch
    | CallCustomIcon(typeName, parameters) ->
        let newContext =
            List.map (fun parameter -> lazy instructionParamEval context parameter) parameters
            |> createContextForCustomIcon context typeName
        eval newContext (getContextEntryPointInstruction newContext)

and instructionParamEval (context : EvalContext) (instruction : IconInstructionParameter) =
    match instruction with
    | Trap -> trap context
    | Constant n -> n
    | BaseIconParameter index -> context.Parameters[index].Value
    | LocalIconInstructionReference id  -> eval {context with CurrentIconID = id } (getIconInstructionFromID context id)