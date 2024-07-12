module PygmalionReimplementation.Eval

open PygmalionReimplementation.Icons
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

type EvalContext =
    { CustomOperations : CustomOperations
      ExecutingCustomOperationName : string
      CurrentIconID : IconID option
      Parameters : int list
      RecursionDepth : int }

let evalUnary operator rawOperand paramEval results =
    let (operand, resultsTable) = paramEval results rawOperand
    match operator with
    | "!" -> if operand = FalseValue then TrueValue else FalseValue
    | "-" -> -operand
    | "+" -> operand
    | _ -> failwith "Unknown unary operator"
    |> fun result -> (result, resultsTable)

let evalBinary operator rawOperand1 rawOperand2 paramEval results =
    let (operand1, resultsTableFirst) = paramEval results rawOperand1
    let (operand2, resultsTableSecond) = paramEval resultsTableFirst rawOperand2

    getCorrectBinaryOperation operator operand1 operand2
    |> fun result -> (result, resultsTableSecond)


let getCustomOperation (context : EvalContext) =
    context.CustomOperations.[context.ExecutingCustomOperationName]

exception TrapException of EvalContext * IconResultsTable

let evalCustomOperation
    (oldContext : EvalContext)
    customOperationName
    customOperationParameters
    (iconEval : EvalContext -> IconResultsTable -> IconID -> IconResultsTable)
    : int =
    let customOperation = oldContext.CustomOperations[customOperationName]
    let nextIconID = customOperation.EntryPointIcon

    printf "Entry point icon: %A\n" nextIconID

    let newContext =
        { oldContext with
            ExecutingCustomOperationName = customOperationName
            CurrentIconID = nextIconID
            Parameters = customOperationParameters
            RecursionDepth = oldContext.RecursionDepth + 1}

    if Option.isNone(nextIconID) then raise (TrapException(newContext, Map.empty))

    let nextIconActualID = nextIconID.Value
    iconEval newContext Map.empty nextIconActualID
    |> fun results -> results[nextIconActualID]

let getIconInstructionFromID (context : EvalContext) (id : IconID) =
    getCustomOperation context
    |> fun customOperation -> customOperation.SavedIcons[id].Operation


let trap (context : EvalContext) (results : IconResultsTable) =
    TrapException(context, results)
    |> raise

let eval (context : EvalContext) (targetID : IconID) =
    let rec internalEval (context : EvalContext) (results : IconResultsTable) (targetID : IconID) : IconResultsTable =
        let boundParamEval = iconParameterEval context
        let saveOutput (output : int * IconResultsTable) : IconResultsTable =
            let (result, resultsTable) = output
            resultsTable.Add(targetID, result)
        let saveCustomOperationEvalOutput (newResults : IconResultsTable) (output : int) : IconResultsTable =
            newResults.Add(targetID, output)

        match getIconInstructionFromID context targetID with
        | Unary(operator, operand) ->
            evalUnary operator operand boundParamEval results
            |> saveOutput
        | Binary(operator, operand1, operand2) ->
            evalBinary operator operand1 operand2 boundParamEval results
            |> saveOutput
        | If(condition) ->
            let trueName, falseName = getCustomIfIconNames context.ExecutingCustomOperationName targetID
            let (conditionResult, newResultsTable) = boundParamEval results condition
            let targetCustomOperationName = if conditionResult = FalseValue then falseName else trueName

            evalCustomOperation context targetCustomOperationName context.Parameters internalEval
            |> saveCustomOperationEvalOutput newResultsTable
        | CallCustomOperation(typeName, parameters) ->
            let (evaluatedParameters, newResultsTable) =
                List.foldBack
                    (fun parameter (evalResults, resultsTable) ->
                        let (result, newResultsTable) = boundParamEval resultsTable parameter
                        (result :: evalResults, newResultsTable))
                    parameters
                    ([], results)
            evalCustomOperation context typeName evaluatedParameters internalEval
            |> saveCustomOperationEvalOutput newResultsTable
        | TopLevelTrap -> trap context results

    and iconParameterEval (context : EvalContext) (results : IconResultsTable) (parameter : IconOperationParameter) : int * IconResultsTable =
        let wrapOutput (output : int) =
            (output, results)
        match parameter with
        | Constant value -> wrapOutput value
        | OperationParameter index ->
            let parameterValue = context.Parameters.[index]
            wrapOutput parameterValue
        | LocalIconReference id ->
            match results.TryFind id with
            | Some result -> wrapOutput result
            | None ->
                internalEval context results id
                |> fun res -> (res[id], res)
        | Trap -> trap context results

    internalEval context Map.empty targetID