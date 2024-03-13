module PygmalionReimplementation.Icons

open Aether
open Aether.Operators

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

and IconType =
    | BaseUnaryIcon of operation : UnaryOperation
    | BaseBinaryIcon of operation : BinaryOperation
    | BaseIfIcon
    | CustomIcon of customIconName : CustomIconPrism * parameterCount : int

and DrawnIcon =
    { X : int
      Y : int
      IconInstruction : IconInstruction
      Result : UnderlyingNumberDataType option }

    static member X_ =
        _.X, (fun newValue instance -> { instance with X = newValue })
    static member Y_ =
        _.Y, (fun newValue instance -> { instance with Y = newValue })
    static member IconInstruction_ =
        _.IconInstruction, (fun newValue instance -> { instance with IconInstruction = newValue })
    static member Result_ =
        _.Result, (fun newValue instance -> { instance with Result = newValue })

and DrawnIcons = DrawnIcon list
and DrawnIconPrism = Prism<DrawnIcons, DrawnIcon>

and MovableObject =
    | NoObject
    | ExistingIcon of DrawnIconPrism
    | NewIcon of IconType
    | Number of UnderlyingNumberDataType

and MovableObjectTarget =
    | Position of x : int * y : int
    | IconParameter of target : DrawnIconPrism * position : int
    | ExecutionResult

and CustomIcon =
    { ParameterCount : int
      ActionTree : ExecutionActionTree }

    static member ParameterCount_ =
        _.ParameterCount, (fun newValue instance -> { instance with ParameterCount = newValue })
    static member LocalActionTree_ =
        _.ActionTree, (fun newValue instance -> { instance with ActionTree = newValue })

and SimpleExecutionAction =
    | EvaluateIcon of DrawnIconPrism
    | PickupNewIcon of IconType
    | PickupIcon of DrawnIconPrism
    | PickupNumber of UnderlyingNumberDataType
    | PickupExecutionStateParameter of parameterIndex : int
    | PlacePickup of MovableObjectTarget
    | CancelPickup
    | RemoveIcon of remover : (DrawnIcons -> DrawnIcons)
    | RemoveIconParameter of target : DrawnIconPrism * position : int

and BranchingExecutionAction =
    | EvaluateIf of DrawnIconPrism

and ExecutionActionTree =
    | Linear of action : SimpleExecutionAction * next : ExecutionActionTree
    | Branch of action : BranchingExecutionAction * falseBranch : ExecutionActionTree * trueBranch : ExecutionActionTree
    | End

and CustomIconName = string
and CustomIcons = Map<CustomIconName, CustomIcon>
and CustomIconPrism = Prism<CustomIcons, CustomIcon>

let initialCustomIconName = "_main"

type ExecutionState =
    { HeldObject : MovableObject
      LocalIcons : DrawnIcons
      CurrentBranchChoices : bool list
      Parameters : UnderlyingNumberDataType list
      Result : UnderlyingNumberDataType option }

    static member HeldObject_ =
        _.HeldObject, (fun newValue instance -> { instance with HeldObject = newValue})
    static member LocalIcons_ =
        _.LocalIcons, (fun newValue instance -> { instance with LocalIcons = newValue })
    static member CurrentBranchChoices_ =
        _.CurrentBranchChoices , (fun newValue instance -> { instance with CurrentBranchChoices = newValue })
    static member Parameters_ =
        _.Parameters, (fun newValue instance -> { instance with Parameters = newValue })
    static member Result_ =
        _.Result, (fun newValue instance -> { instance with ExecutionState.Result = newValue })

let baseExecutionState parameters =
    { HeldObject = NoObject
      LocalIcons = List.empty
      CurrentBranchChoices = List.empty
      Parameters = parameters
      Result = None }

let private invalidCustomIconNameCharacters = "\t\n\r_"

let Trap : IconInstructionParameter = None

exception ExtendedTrapException of CustomIconPrism * UnderlyingNumberDataType list

let transformInstructionParameters transform =
    transform ^% IconInstruction.Params_ // Optic.map

let replaceParameter position newParameter instruction =
    let transform = listReplaceIndex position newParameter
    transformInstructionParameters transform instruction

let createEmptyIconInstruction (iconType : IconType) =
    match iconType with
    | BaseUnaryIcon op -> Unary(op, Trap)
    | BaseBinaryIcon op -> Binary(op, Trap, Trap)
    | BaseIfIcon -> If(Trap)
    | CustomIcon(customIconPrism, paramCount) -> CallCustomIcon(customIconPrism, List.init paramCount (fun _ -> Trap))

let createEmptyDrawnIcon x y instruction =
    { IconInstruction = instruction
      Result = None
      X = x
      Y = y }

let customIconNameContainsInvalidCharacter (name : string) =
    name |> Seq.exists (fun c -> invalidCustomIconNameCharacters.Contains(c))

let private setParameterAtPosition targetPrism position newParameter =
    let fullTargetOptic =
        ExecutionState.LocalIcons_ >-> targetPrism >?> DrawnIcon.IconInstruction_
    (replaceParameter position newParameter) ^% fullTargetOptic

let private placePickup target state =
    let createNewIconAt x y iconType =
        let newDrawnIcon =
            createEmptyIconInstruction iconType |> createEmptyDrawnIcon x y
        state |> cons newDrawnIcon ^% ExecutionState.LocalIcons_

    let placeIconAt x y targetPrism =
        let fullIconPrism = ExecutionState.LocalIcons_ >-> targetPrism
        state
        |> x ^= (fullIconPrism >?> DrawnIcon.X_)
        |> y ^= (fullIconPrism >?> DrawnIcon.Y_)

    let heldObject = state ^. ExecutionState.HeldObject_
    match (target, heldObject) with
    | (Position (x, y), NewIcon iconType) ->
        createNewIconAt x y iconType
    | (Position (x, y), ExistingIcon iconPrism) ->
        placeIconAt x y iconPrism
    | (IconParameter (targetPrism, position), Number number) ->
        setParameterAtPosition targetPrism position (Some number) state
    | (ExecutionResult, Number number) ->
        {state with Result = Some number}
    | (_, _) -> state

let rec private evaluateIconInstruction customIcons iconInstruction =
    match iconInstruction with
    | Unary(op, arg) ->
        evalUnaryOperation op arg
    | Binary(op, arg1, arg2) ->
        evalBinaryOperation op arg1 arg2
    | If(op) ->
        Option.defaultWith (raise TrapException) op
    | CallCustomIcon(iconPrism: CustomIconPrism, parameters) ->
        parameters
        |> List.map (Option.defaultWith (raise TrapException))
        |> buildExecutionStateForCustomIcon customIcons iconPrism
        |> Optic.get ExecutionState.Result_
        |> Option.defaultWith (raise TrapException) // Trap if result wasn't set

and private evaluateIconInstance customIcons drawnIcon =
    let iconInstruction = drawnIcon.IconInstruction
    let result = evaluateIconInstruction customIcons iconInstruction
    {drawnIcon with Result = Some result}

and private evaluateIcon customIcons iconPrism : ExecutionState -> ExecutionState =
    evaluateIconInstance customIcons ^% (ExecutionState.LocalIcons_ >-> iconPrism)

and private evaluateIf customIcons ifPrism state =
    let newState = evaluateIcon customIcons ifPrism state
    let result =
        let resultPrism = ExecutionState.LocalIcons_ >-> ifPrism >?> DrawnIcon.Result_
        newState ^. resultPrism |> Option.get |> Option.get |> intToBool
    newState |> cons result ^% ExecutionState.CurrentBranchChoices_

and private applyExecutionActionNodeInternal customIcons (actionNode : ExecutionActionTree) =
    let applySimpleExecutionAction customIcons action =
        match action with
        | EvaluateIcon iconPrism ->
            evaluateIcon customIcons iconPrism
        | PickupNewIcon iconType ->
            (NewIcon iconType) ^= ExecutionState.HeldObject_
        | PickupIcon iconPrism->
            (ExistingIcon iconPrism) ^= ExecutionState.HeldObject_
        | PickupNumber parameter ->
            (Number parameter) ^= ExecutionState.HeldObject_
        | PickupExecutionStateParameter parameterIndex ->
            fun state ->
                let parameterOptic = ExecutionState.Parameters_ >-> List.pos_ parameterIndex
                let parameter = Option.get (state ^. parameterOptic) // Crashes on invalid index
                state |> (Number parameter) ^= ExecutionState.HeldObject_
        | CancelPickup ->
            NoObject ^= ExecutionState.HeldObject_
        | PlacePickup target ->
            placePickup target
        | RemoveIcon remover ->
            remover ^% ExecutionState.LocalIcons_
        | RemoveIconParameter (targetPrism, position) ->
            setParameterAtPosition targetPrism position Trap
    let applyBranchingExecutionAction customIcons action =
        match action with
        | EvaluateIf (ifPrism) ->
            evaluateIf customIcons ifPrism

    match actionNode with
    | Linear (action, _) ->
        applySimpleExecutionAction customIcons action
    | Branch (action, _, _) ->
        applyBranchingExecutionAction customIcons action
    | End -> id

and private applyExecutionActionTree customIcons (actionTree : ExecutionActionTree) state =
    let stateWithAppliedHeadAction = applyExecutionActionNodeInternal customIcons actionTree state
    let boundRecursiveCall next =
        applyExecutionActionTree customIcons next stateWithAppliedHeadAction
    match actionTree with
    | Linear (_, next) -> boundRecursiveCall next
    | Branch (_, falseBranch, trueBranch) ->
        let nextBranch = if stateWithAppliedHeadAction.CurrentBranchChoices.Head then trueBranch else falseBranch
        boundRecursiveCall nextBranch
    | End -> stateWithAppliedHeadAction

and buildExecutionStateForCustomIcon customIcons (customIconOptic : CustomIconPrism) parameters =
    let baseExecutionState = baseExecutionState parameters
    let actionTree=
        let optic = customIconOptic >?> CustomIcon.LocalActionTree_
        customIcons ^. optic |> Option.get
    try
        applyExecutionActionTree customIcons actionTree baseExecutionState
    with TrapException ->
        let outerException = ExtendedTrapException(customIconOptic, parameters)
        raise outerException

let applyExecutionActionNode customIcons actionNode state=
    try
        applyExecutionActionNodeInternal customIcons actionNode state
    with TrapException ->
        state
let appendNewActionToTree newAction choicesList (actionTree : ExecutionActionTree) =
    let rec appendNewActionToTree' newAction choicesList actionTree =
        let boundRecursiveCall = appendNewActionToTree' newAction
        match actionTree with
        | Linear (simpleAction, next) ->
            Linear(simpleAction, boundRecursiveCall choicesList next)
        | Branch (branchingAction, falseBranch, trueBranch) ->
            match choicesList with
            | false :: restChoices ->
                Branch(branchingAction, boundRecursiveCall restChoices falseBranch, trueBranch)
            | true :: restChoices ->
                Branch(branchingAction, falseBranch, boundRecursiveCall restChoices trueBranch)
            | [] -> failwith "Missing choice"
        | End -> newAction

    appendNewActionToTree' newAction (List.rev choicesList) actionTree

let wrapSimpleExecutionAction action =
    Linear(action, End)

let wrapBranchingExecutionAction action =
    Branch(action, End, End)