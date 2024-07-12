module PygmalionReimplementation.Icons

open Aether
open Aether.Operators

open System
open PygmalionReimplementation.Utils
open PygmalionReimplementation.SimpleEval

type IconOperation =
    | Unary of operator : UnaryOperation * IconOperationParameter
    | Binary of operator : BinaryOperation * IconOperationParameter * IconOperationParameter
    | If of IconOperationParameter
    | CallCustomOperation of customOperationName : CustomOperationPrism * IconOperationParameter list

    static member Params_ =
        let view operation =
            match operation with
            | Unary(_, param) -> [ param ]
            | Binary(_, param1, param2) -> [ param1; param2 ]
            | If(arg) -> [ arg; ]
            | CallCustomOperation (_, parameters) -> parameters
        let update (parameters : IconOperationParameter list) operation =
            match operation with
            | Unary(op, _) -> Unary(op, parameters[0])
            | Binary(op, _, _) -> Binary(op, parameters[0], parameters[1])
            | If _ -> If(parameters[0])
            | CallCustomOperation (iconType, _) -> CallCustomOperation(iconType, parameters)
        (view, update)

and Icon =
    { X : int
      Y : int
      Z : int // Used for drawing overlapping icons
      Operation : IconOperation
      Result : UnderlyingNumberDataType option }

    static member X_ =
        _.X, (fun newValue icon -> { icon with X = newValue })
    static member Y_ =
        _.Y, (fun newValue icon -> { icon with Y = newValue })
    static member Z_ =
        _.Z, (fun newValue icon -> { icon with Z = newValue })
    static member Operation_ =
        _.Operation, (fun newValue icon -> { icon with Operation = newValue })
    static member Result_ =
        _.Result, (fun newValue icon -> { icon with Result = newValue })

and Icons = Icon list
and IconPrism = Prism<Icons, Icon>

and MovableObject =
    | NoObject
    | ExistingIcon of IconPrism
    | NewIcon of IconOperation
    | Number of UnderlyingNumberDataType

and MovableObjectTarget =
    | Position of x : int * y : int
    | IconParameter of target : IconPrism * position : int

and CustomOperation =
    { Name : string
      ParameterCount : int
      ActionTree : ExecutionActionTree }

    static member Name_ =
        _.Name, (fun newValue icon -> { icon with CustomOperation.Name = newValue })
    static member ParameterCount_ =
        _.ParameterCount, (fun newValue icon -> { icon with ParameterCount = newValue })
    static member LocalActionTree_ =
        _.ActionTree, (fun newValue icon -> { icon with ActionTree = newValue })

and SimpleExecutionAction =
    | EvaluateSimpleIcon of IconPrism
    | PickupNewIcon of IconOperation
    | PickupIcon of IconPrism
    | PickupNumber of UnderlyingNumberDataType
    | PickupExecutionStateParameter of parameterIndex : int
    | PickupIconResult of IconPrism
    | PlacePickup of MovableObjectTarget
    | CancelPickup
    | RemoveIcon of remover : (Icons -> Icons)
    | RemoveIconParameter of target : IconPrism * position : int

and BranchingExecutionAction =
    | EvaluateBranchingIcon of IconPrism

and FinalExecutionAction =
    | SaveResult
    | Trap

and ExecutionActionTree =
    | Linear of action : SimpleExecutionAction * next : ExecutionActionTree
    | Branch of action : BranchingExecutionAction * falseBranch : ExecutionActionTree * trueBranch : ExecutionActionTree
    | End of action : FinalExecutionAction


and CustomOperationIndex = int
and CustomOperations = CustomOperation list
and CustomOperationPrism = Prism<CustomOperations, CustomOperation>

let initialCustomOperationName = "Main"
let initialCustomOperationIndex = 0

type ExecutionState =
    { HeldObject : MovableObject
      LocalIcons : Icons
      CurrentBranchChoices : bool list
      Parameters : UnderlyingNumberDataType list
      Result : UnderlyingNumberDataType option
      LastZ : int }

    static member HeldObject_ =
        _.HeldObject, (fun newValue icon -> { icon with HeldObject = newValue})
    static member LocalIcons_ =
        _.LocalIcons, (fun newValue icon -> { icon with LocalIcons = newValue })
    static member CurrentBranchChoices_ =
        _.CurrentBranchChoices, (fun newValue icon -> { icon with CurrentBranchChoices = newValue })
    static member Parameters_ =
        _.Parameters, (fun newValue icon -> { icon with Parameters = newValue })
    static member Result_ =
        _.Result, (fun newValue icon -> { icon with ExecutionState.Result = newValue })
    static member LastZ_ =
        _.LastZ, (fun newValue icon -> { icon with LastZ = newValue })

let baseExecutionState parameters =
    { HeldObject = NoObject
      LocalIcons = List.empty
      CurrentBranchChoices = List.empty
      Parameters = parameters
      Result = None
      LastZ = 0}

let private invalidCustomOperationNameCharacters = "\t\n\r_"

exception private InternalRecursionTrapException of ExecutionState
exception RecursionTrapException of CustomOperationPrism * UnderlyingNumberDataType list * ExecutionState

let private incrementLastZ =
    (+) 1 ^% ExecutionState.LastZ_

let transformOperationParameters transform =
    transform ^% IconOperation.Params_ // Optic.map

let replaceParameter position newParameter operation =
    let transform = listReplaceIndex position newParameter
    transformOperationParameters transform operation

let createIcon x y z operation =
    { Operation = operation
      Result = None
      X = x
      Y = y
      Z = z }

let customOperationNameContainsInvalidCharacter (name : string) =
    name |> Seq.exists (fun c -> invalidCustomOperationNameCharacters.Contains(c))

let private setParameterAtPosition targetPrism position newParameter =
    let fullTargetOptic =
        ExecutionState.LocalIcons_ >-> targetPrism >?> Icon.Operation_
    (replaceParameter position newParameter) ^% fullTargetOptic

let private placePickup target state =
    let dropPickup =
        NoObject ^= ExecutionState.HeldObject_

    let createNewIconAt x y operation =
        let z = state.LastZ
        let newIcon = createIcon x y z operation
        state
        |> cons newIcon ^% ExecutionState.LocalIcons_
        |> incrementLastZ

    let placeIconAt x y targetPrism =
        let fullIconPrism = ExecutionState.LocalIcons_ >-> targetPrism
        let z  = state.LastZ
        state
        |> x ^= (fullIconPrism >?> Icon.X_)
        |> y ^= (fullIconPrism >?> Icon.Y_)
        |> z ^= (fullIconPrism >?> Icon.Z_)
        |> incrementLastZ

    let heldObject = state ^. ExecutionState.HeldObject_
    match (target, heldObject) with
    | (Position (x, y), NewIcon operation) ->
        createNewIconAt x y operation |> dropPickup
    | (Position (x, y), ExistingIcon iconPrism) ->
        placeIconAt x y iconPrism |> dropPickup
    | (IconParameter (targetPrism, position), Number number) ->
        setParameterAtPosition targetPrism position (Some number) state |> dropPickup
    | (_, _) -> state

let rec private evaluateIconOperation customOperations iconOperation =
    match iconOperation with
    | Unary(op, arg) ->
        evalUnaryOperation op arg
    | Binary(op, arg1, arg2) ->
        evalBinaryOperation op arg1 arg2
    | If op -> op
    | CallCustomOperation(iconPrism: CustomOperationPrism, parameters) ->
        let allParametersPresent = parameters |> List.tryFind Option.isNone |> Option.isNone
        if allParametersPresent then
            buildExecutionStateForCustomOperation customOperations iconPrism (List.map Option.get parameters)
            |> Optic.get ExecutionState.Result_
        else
            None

and private appendIfResultToState ifResult =
    cons (intToBool ifResult) ^% ExecutionState.CurrentBranchChoices_

and private evaluateIcon customOperations icon =
    let operation = icon.Operation
    let result = evaluateIconOperation customOperations operation
    {icon with Result = result}

and private evaluateIconFromPrism customOperations iconPrism =
    evaluateIcon customOperations ^% (ExecutionState.LocalIcons_ >-> iconPrism)

and private evaluateBranchingIcon customOperations iconPrism state =
    let stateWithEvaluatedIcon = evaluateIconFromPrism customOperations iconPrism state
    let icon =  stateWithEvaluatedIcon ^. (ExecutionState.LocalIcons_ >-> iconPrism)
    match icon with
    | Some { Operation = If _; Result = Some result } ->
        stateWithEvaluatedIcon |> appendIfResultToState result
    | _ -> stateWithEvaluatedIcon

and applyExecutionActionNode customOperations (actionNode : ExecutionActionTree) state =
    let applySimpleExecutionAction customOperations action =
        match action with
        | EvaluateSimpleIcon iconPrism ->
            evaluateIconFromPrism customOperations iconPrism
        | PickupNewIcon operation ->
            (NewIcon operation) ^= ExecutionState.HeldObject_
        | PickupIcon iconPrism ->
            (ExistingIcon iconPrism) ^= ExecutionState.HeldObject_
        | PickupNumber parameter ->
            (Number parameter) ^= ExecutionState.HeldObject_
        | PickupExecutionStateParameter parameterIndex ->
            fun state ->
                let parameterOptic = ExecutionState.Parameters_ >-> List.pos_ parameterIndex
                let parameter = Option.get (state ^. parameterOptic) // Crashes on invalid index
                state |> (Number parameter) ^= ExecutionState.HeldObject_
        | PickupIconResult iconPrism ->
            fun state ->
                let iconOptic = ExecutionState.LocalIcons_ >-> iconPrism >?> Icon.Result_
                let result = state ^. iconOptic |> Option.flatten |> Option.defaultValue 0
                state |> (Number result) ^= ExecutionState.HeldObject_
        | CancelPickup ->
            NoObject ^= ExecutionState.HeldObject_
        | PlacePickup target ->
            placePickup target
        | RemoveIcon remover ->
            remover ^% ExecutionState.LocalIcons_
        | RemoveIconParameter (targetPrism, position) ->
            setParameterAtPosition targetPrism position None
    let applyBranchingExecutionAction customOperations action state =
        match action with
        | EvaluateBranchingIcon iconPrism ->
            evaluateBranchingIcon customOperations iconPrism state
    let applyFinalExecutionAction action state =
        let saveResult state =
            match state.HeldObject with
            | Number result -> {state with Result = Some result}
            | _ -> InvalidOperationException("Tried to save invalid object to result") |> raise
        match action with
        | Trap -> InternalRecursionTrapException state |> raise
        | SaveResult ->
            saveResult state

    match actionNode with
    | Linear (action, _) ->
        applySimpleExecutionAction customOperations action state
    | Branch (action, _, _) ->
        applyBranchingExecutionAction customOperations action state
    | End action ->
        applyFinalExecutionAction action state

and private applyExecutionActionTree customOperations (actionTree : ExecutionActionTree) state =
    let stateWithAppliedHeadAction = applyExecutionActionNode customOperations actionTree state
    let boundRecursiveCall next =
        applyExecutionActionTree customOperations next stateWithAppliedHeadAction
    match actionTree with
    | Linear (_, next) -> boundRecursiveCall next
    | Branch (_, falseBranch, trueBranch) ->
        let nextBranch = if stateWithAppliedHeadAction.CurrentBranchChoices.Head then trueBranch else falseBranch
        boundRecursiveCall nextBranch
    | End _ ->
        stateWithAppliedHeadAction

and buildExecutionStateForCustomOperation customOperations (customOperationOptic : CustomOperationPrism) parameters =
    let baseExecutionState = baseExecutionState parameters
    let actionTree=
        let optic = customOperationOptic >?> CustomOperation.LocalActionTree_
        customOperations ^. optic |> Option.get

    try
        applyExecutionActionTree customOperations actionTree baseExecutionState
    with InternalRecursionTrapException newState ->
        RecursionTrapException(customOperationOptic, parameters, newState) |> raise

let appendNewActionToTree newAction choicesList (actionTree : ExecutionActionTree) =
    let rec appendNewActionToTree' choicesList actionTree =
        match actionTree with
        | Linear (simpleAction, next) ->
            Linear(simpleAction, appendNewActionToTree' choicesList next)
        | Branch (branchingAction, falseBranch, trueBranch) ->
            match choicesList with
            | false :: restChoices ->
                Branch(branchingAction, appendNewActionToTree' restChoices falseBranch, trueBranch)
            | true :: restChoices ->
                Branch(branchingAction, falseBranch, appendNewActionToTree' restChoices trueBranch)
            | [] -> failwith "Missing choice"
        | End Trap -> newAction
        | End SaveResult -> InvalidOperationException("Tried to replace action of saving the result") |> raise

    appendNewActionToTree' (List.rev choicesList) actionTree

let defaultEnd = End Trap
let wrapSimpleExecutionAction action =
    Linear(action, defaultEnd)

let wrapBranchingExecutionAction action =
    Branch(action, defaultEnd, defaultEnd)

let resultSaveAction =
    End SaveResult

let isCustomOperationNameValid name =
    isText name && not (customOperationNameContainsInvalidCharacter name)
