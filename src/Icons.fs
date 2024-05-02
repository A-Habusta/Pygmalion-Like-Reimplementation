module PygmalionReimplementation.Icons

open Aether
open Aether.Operators

open System
open PygmalionReimplementation.Utils
open PygmalionReimplementation.SimpleEval

type IconInstruction =
    | Unary of operator : UnaryOperation * IconInstructionParameter
    | Binary of operator : BinaryOperation * IconInstructionParameter * IconInstructionParameter
    | If of IconInstructionParameter
    | CallCustomIcon of customIconName : CustomIconPrism * IconInstructionParameter list

    static member Params_ =
        let view instruction =
            match instruction with
            | Unary(_, param) -> [ param ]
            | Binary(_, param1, param2) -> [ param1; param2 ]
            | If(arg) -> [ arg; ]
            | CallCustomIcon (_, parameters) -> parameters
        let update (parameters : IconInstructionParameter list) instruction =
            match instruction with
            | Unary(op, _) -> Unary(op, parameters[0])
            | Binary(op, _, _) -> Binary(op, parameters[0], parameters[1])
            | If _ -> If(parameters[0])
            | CallCustomIcon (iconType, _) -> CallCustomIcon(iconType, parameters)
        (view, update)

and DrawnIcon =
    { X : int
      Y : int
      Z : int // Used for drawing overlapping icons
      IconInstruction : IconInstruction
      Result : UnderlyingNumberDataType option }

    static member X_ =
        _.X, (fun newValue instance -> { instance with X = newValue })
    static member Y_ =
        _.Y, (fun newValue instance -> { instance with Y = newValue })
    static member Z_ =
        _.Z, (fun newValue instance -> { instance with Z = newValue })
    static member IconInstruction_ =
        _.IconInstruction, (fun newValue instance -> { instance with IconInstruction = newValue })
    static member Result_ =
        _.Result, (fun newValue instance -> { instance with Result = newValue })

and DrawnIcons = DrawnIcon list
and DrawnIconPrism = Prism<DrawnIcons, DrawnIcon>

and MovableObject =
    | NoObject
    | ExistingIcon of DrawnIconPrism
    | NewIcon of IconInstruction
    | Number of UnderlyingNumberDataType

and MovableObjectTarget =
    | Position of x : int * y : int
    | IconParameter of target : DrawnIconPrism * position : int

and CustomIcon =
    { Name : string
      ParameterCount : int
      ActionTree : ExecutionActionTree }

    static member Name_ =
        _.Name, (fun newValue instance -> { instance with CustomIcon.Name = newValue })
    static member ParameterCount_ =
        _.ParameterCount, (fun newValue instance -> { instance with ParameterCount = newValue })
    static member LocalActionTree_ =
        _.ActionTree, (fun newValue instance -> { instance with ActionTree = newValue })

and SimpleExecutionAction =
    | EvaluateSimpleIcon of DrawnIconPrism
    | PickupNewIcon of IconInstruction
    | PickupIcon of DrawnIconPrism
    | PickupNumber of UnderlyingNumberDataType
    | PickupExecutionStateParameter of parameterIndex : int
    | PickupIconResult of DrawnIconPrism
    | PlacePickup of MovableObjectTarget
    | CancelPickup
    | RemoveIcon of remover : (DrawnIcons -> DrawnIcons)
    | RemoveIconParameter of target : DrawnIconPrism * position : int

and BranchingExecutionAction =
    | EvaluateBranchingIcon of DrawnIconPrism

and FinalExecutionAction =
    | SaveResult
    | Trap

and ExecutionActionTree =
    | Linear of action : SimpleExecutionAction * next : ExecutionActionTree
    | Branch of action : BranchingExecutionAction * falseBranch : ExecutionActionTree * trueBranch : ExecutionActionTree
    | End of action : FinalExecutionAction


and CustomIconIndex = int
and CustomIcons = CustomIcon list
and CustomIconPrism = Prism<CustomIcons, CustomIcon>

let initialCustomIconName = "Main"
let initialCustomIconIndex = 0

type ExecutionState =
    { HeldObject : MovableObject
      LocalIcons : DrawnIcons
      CurrentBranchChoices : bool list
      Parameters : UnderlyingNumberDataType list
      Result : UnderlyingNumberDataType option
      LastZ : int }

    static member HeldObject_ =
        _.HeldObject, (fun newValue instance -> { instance with HeldObject = newValue})
    static member LocalIcons_ =
        _.LocalIcons, (fun newValue instance -> { instance with LocalIcons = newValue })
    static member CurrentBranchChoices_ =
        _.CurrentBranchChoices, (fun newValue instance -> { instance with CurrentBranchChoices = newValue })
    static member Parameters_ =
        _.Parameters, (fun newValue instance -> { instance with Parameters = newValue })
    static member Result_ =
        _.Result, (fun newValue instance -> { instance with ExecutionState.Result = newValue })
    static member LastZ_ =
        _.LastZ, (fun newValue instance -> { instance with LastZ = newValue })

let baseExecutionState parameters =
    { HeldObject = NoObject
      LocalIcons = List.empty
      CurrentBranchChoices = List.empty
      Parameters = parameters
      Result = None
      LastZ = 0}

let private invalidCustomIconNameCharacters = "\t\n\r_"

exception private InternalRecursionTrapException of ExecutionState
exception RecursionTrapException of CustomIconPrism * UnderlyingNumberDataType list * ExecutionState

let private incrementLastZ =
    (+) 1 ^% ExecutionState.LastZ_

let transformInstructionParameters transform =
    transform ^% IconInstruction.Params_ // Optic.map

let replaceParameter position newParameter instruction =
    let transform = listReplaceIndex position newParameter
    transformInstructionParameters transform instruction

let createDrawnIcon x y z instruction =
    { IconInstruction = instruction
      Result = None
      X = x
      Y = y
      Z = z }

let customIconNameContainsInvalidCharacter (name : string) =
    name |> Seq.exists (fun c -> invalidCustomIconNameCharacters.Contains(c))

let private setParameterAtPosition targetPrism position newParameter =
    let fullTargetOptic =
        ExecutionState.LocalIcons_ >-> targetPrism >?> DrawnIcon.IconInstruction_
    (replaceParameter position newParameter) ^% fullTargetOptic

let private placePickup target state =
    let dropPickup =
        NoObject ^= ExecutionState.HeldObject_

    let createNewIconAt x y instruction =
        let z = state.LastZ
        let newDrawnIcon = createDrawnIcon x y z instruction
        state
        |> cons newDrawnIcon ^% ExecutionState.LocalIcons_
        |> incrementLastZ

    let placeIconAt x y targetPrism =
        let fullIconPrism = ExecutionState.LocalIcons_ >-> targetPrism
        let z  = state.LastZ
        state
        |> x ^= (fullIconPrism >?> DrawnIcon.X_)
        |> y ^= (fullIconPrism >?> DrawnIcon.Y_)
        |> z ^= (fullIconPrism >?> DrawnIcon.Z_)
        |> incrementLastZ

    let heldObject = state ^. ExecutionState.HeldObject_
    match (target, heldObject) with
    | (Position (x, y), NewIcon instruction) ->
        createNewIconAt x y instruction |> dropPickup
    | (Position (x, y), ExistingIcon iconPrism) ->
        placeIconAt x y iconPrism |> dropPickup
    | (IconParameter (targetPrism, position), Number number) ->
        setParameterAtPosition targetPrism position (Some number) state |> dropPickup
    | (_, _) -> state

let rec private evaluateIconInstruction customIcons iconInstruction =
    match iconInstruction with
    | Unary(op, arg) ->
        evalUnaryOperation op arg
    | Binary(op, arg1, arg2) ->
        evalBinaryOperation op arg1 arg2
    | If op -> op
    | CallCustomIcon(iconPrism: CustomIconPrism, parameters) ->
        let allParametersPresent = parameters |> List.tryFind Option.isNone |> Option.isNone
        if allParametersPresent then
            buildExecutionStateForCustomIcon customIcons iconPrism (List.map Option.get parameters)
            |> Optic.get ExecutionState.Result_
        else
            None

and private appendIfResultToState ifResult =
    cons (intToBool ifResult) ^% ExecutionState.CurrentBranchChoices_

and private evaluateIconInstance customIcons drawnIcon =
    let iconInstruction = drawnIcon.IconInstruction
    let result = evaluateIconInstruction customIcons iconInstruction
    {drawnIcon with Result = result}

and private evaluateIcon customIcons iconPrism =
    evaluateIconInstance customIcons ^% (ExecutionState.LocalIcons_ >-> iconPrism)

and private evaluateBranchingIcon customIcons iconPrism state =
    let stateWithEvaluatedIcon = evaluateIcon customIcons iconPrism state
    let icon =  stateWithEvaluatedIcon ^. (ExecutionState.LocalIcons_ >-> iconPrism)
    match icon with
    | Some { IconInstruction = If _; Result = Some result } ->
        stateWithEvaluatedIcon |> appendIfResultToState result
    | _ -> stateWithEvaluatedIcon

and applyExecutionActionNode customIcons (actionNode : ExecutionActionTree) state =
    let applySimpleExecutionAction customIcons action =
        match action with
        | EvaluateSimpleIcon iconPrism ->
            evaluateIcon customIcons iconPrism
        | PickupNewIcon instruction ->
            (NewIcon instruction) ^= ExecutionState.HeldObject_
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
                let iconOptic = ExecutionState.LocalIcons_ >-> iconPrism >?> DrawnIcon.Result_
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
    let applyBranchingExecutionAction customIcons action state =
        match action with
        | EvaluateBranchingIcon iconPrism ->
            evaluateBranchingIcon customIcons iconPrism state
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
        applySimpleExecutionAction customIcons action state
    | Branch (action, _, _) ->
        applyBranchingExecutionAction customIcons action state
    | End action ->
        applyFinalExecutionAction action state

and private applyExecutionActionTree customIcons (actionTree : ExecutionActionTree) state =
    let stateWithAppliedHeadAction = applyExecutionActionNode customIcons actionTree state
    let boundRecursiveCall next =
        applyExecutionActionTree customIcons next stateWithAppliedHeadAction
    match actionTree with
    | Linear (_, next) -> boundRecursiveCall next
    | Branch (_, falseBranch, trueBranch) ->
        let nextBranch = if stateWithAppliedHeadAction.CurrentBranchChoices.Head then trueBranch else falseBranch
        boundRecursiveCall nextBranch
    | End _ ->
        stateWithAppliedHeadAction

and buildExecutionStateForCustomIcon customIcons (customIconOptic : CustomIconPrism) parameters =
    let baseExecutionState = baseExecutionState parameters
    let actionTree=
        let optic = customIconOptic >?> CustomIcon.LocalActionTree_
        customIcons ^. optic |> Option.get

    try
        applyExecutionActionTree customIcons actionTree baseExecutionState
    with InternalRecursionTrapException newState ->
        RecursionTrapException(customIconOptic, parameters, newState) |> raise

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

let isCustomIconNameValid name =
    isText name && not (customIconNameContainsInvalidCharacter name)
