module SoftwareProject.Eval

open Utils
open Icons

exception TrapException of IconID

type IconContext =
    { TypeLibrary: IconTypeLibrary
      EvaluatedParams: int array
      ID: IconID }

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


let rec eval iconContext instruction =
    let boundEval = (eval iconContext)
    match instruction with
    | Constant n -> n
    | Trap -> raise(TrapException iconContext.ID)
    | Unary(operator, operand) -> evalUnary operator (boundEval operand)
    | Binary(operator, leftOperand, rightOperand) ->
        let operation = getCorrectBinaryOperation operator
        operation (boundEval leftOperand) (boundEval rightOperand)
    | If(cond, trueBranch, falseBranch) ->
        let res = boundEval cond
        if res = FalseValue then boundEval falseBranch
        else boundEval trueBranch
    | IconCall(typeName, ID, parameters) ->
        let evaluatedParameters = Array.map boundEval parameters
        let newContext = { iconContext with
                            EvaluatedParams = evaluatedParameters
                            ID = ID }
        let nextInstruction = fetchIconFromTypeLibrary iconContext.TypeLibrary typeName
        eval newContext nextInstruction.InstructionTree
    | Parameter index -> iconContext.EvaluatedParams[index]