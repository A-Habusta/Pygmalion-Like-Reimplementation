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
    { CustomIcons : CustomIcons
      ExecutingCustomIconName : string
      CurrentIconID : IconID option
      Parameters : Lazy<int> list }

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

let evalIf condition trueBranch falseBranch paramEval results =
    let (conditionValue, resultsTableFirst) = paramEval results condition
    paramEval
        resultsTableFirst
        (if conditionValue = FalseValue then falseBranch else trueBranch)


let getCustomIcon (context : EvalContext) =
    context.CustomIcons.[context.ExecutingCustomIconName]

exception TrapException of EvalContext * IconResultsTable

let evalCustomIcon (context : EvalContext) (customIconName : string) (parameters : Lazy<int> list) iconEval =
    let customIcon = getCustomIcon(context)
    let nextIconID = customIcon.EntryPointIcon
    match nextIconID with
    | None -> raise (TrapException(context, Map.empty))
    | Some id ->
        let newContext =
            { context with
                ExecutingCustomIconName = customIconName
                CurrentIconID = nextIconID
                Parameters = parameters }
        iconEval newContext Map.empty id

let getIconInstructionFromID (context : EvalContext) (id : IconID) =
    getCustomIcon context
    |> fun customIcon -> customIcon.SavedIcons[id].IconInstruction


let trap (context : EvalContext) (results : IconResultsTable) =
    TrapException(context, results)
    |> raise

let createContextForCustomIcon
    (oldContext : EvalContext)
    (typeName : string)
    (parameters : Lazy<int> list) =
        let newIcon = oldContext.CustomIcons[typeName]
        { oldContext with
            ExecutingCustomIconName = typeName
            CurrentIconID = newIcon.EntryPointIcon
            Parameters = parameters }

let eval (context : EvalContext) (targetID : IconID) =
    let rec internalEval (context : EvalContext) (results : IconResultsTable) (targetID : IconID) : IconResultsTable =
        let boundParamEval = iconParameterEval context
        let saveOutput (output : int * IconResultsTable) =
            let (result, resultsTable) = output
            resultsTable.Add(targetID, result)

        match getIconInstructionFromID context targetID with
        | Unary(operator, operand) ->
            evalUnary operator operand boundParamEval results
            |> saveOutput
        | Binary(operator, operand1, operand2) ->
            evalBinary operator operand1 operand2 boundParamEval results
            |> saveOutput
        | If(condition, trueBranch, falseBranch) ->
            evalIf condition trueBranch falseBranch boundParamEval results
            |> saveOutput
        | CallCustomIcon(typeName, parameters) ->
            let lazyParameters =
                parameters
                |> List.map (fun param ->
                    let lazyParam = lazy (boundParamEval results param |> fst)
                    match param with
                    | Constant _ ->
                        ignore (lazyParam.Force ()) // Force evalution since it's a constant
                        lazyParam
                    | _ -> lazyParam)
            let newContext = createContextForCustomIcon context typeName lazyParameters
            evalCustomIcon newContext typeName lazyParameters internalEval
        | TopLevelTrap -> trap context results


    and iconParameterEval (context : EvalContext) (results : IconResultsTable) (parameter : IconInstructionParameter) : int * IconResultsTable =
        let wrapOutput (output : int) =
            (output, results)
        match parameter with
        | Constant value -> wrapOutput value
        | BaseIconParameter index ->
            let parameterValue = context.Parameters.[index].Value
            wrapOutput parameterValue
        | LocalIconInstructionReference id ->
            match results.TryFind id with
            | Some result -> wrapOutput result
            | None ->
                internalEval context results id
                |> fun res -> (res[id], res)
        | Trap -> trap context results

    internalEval context Map.empty targetID