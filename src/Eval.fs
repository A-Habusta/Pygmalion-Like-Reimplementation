module PygmalionReimplementation.Eval

open PygmalionReimplementation.Icons
open PygmalionReimplementation.Utils

exception TrapException of IconID

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
        | "!=" -> (<>)
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
    { CustomIcons : CustomIconTypesMap
      LocalIconInstances : LocalIconCollection
      EvaluatedParams: int list
      CurrentIconID : IconID }

let rec eval (context : EvalContext) (instruction : TopLevelInstruction) =
    let boundChildrenEval = (simpleInstructionEval context)
    match instruction with
    | TopLevelTrap -> raise (TrapException context.CurrentIconID)
    | Unary(operator, operand) -> evalUnary operator (boundChildrenEval operand)
    | Binary(operator, leftOperand, rightOperand) ->
        let operation = getCorrectBinaryOperation operator
        operation (boundChildrenEval leftOperand) (boundChildrenEval rightOperand)
    | If(cond, trueBranch, falseBranch) ->
        let res = boundChildrenEval cond
        if res = FalseValue then boundChildrenEval falseBranch
        else boundChildrenEval trueBranch
    | CallCustomIcon(typeName, parameters) ->
        let evaluatedParameters = List.map boundChildrenEval parameters
        let newIcon = context.CustomIcons[typeName]
        let newContext = { context with
                             LocalIconInstances = newIcon.LocalIcons
                             EvaluatedParams = evaluatedParameters
                             CurrentIconID = newIcon.MainIconID }
        eval newContext newIcon.LocalIcons[newIcon.MainIconID]


and simpleInstructionEval (context : EvalContext) (instruction : SimpleInstruction) =
    match instruction with
    | Trap -> raise (TrapException context.CurrentIconID)
    | Constant n -> n
    | BaseIconParameter index -> context.EvaluatedParams[index]
    | LocalIconReference id -> eval {context with CurrentIconID = id } context.LocalIconInstances[id]