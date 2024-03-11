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
      ActionTree : LocalIconActionTree }

    static member ParameterCount_ =
        _.ParameterCount, (fun newValue instance -> { instance with ParameterCount = newValue })
    static member LocalActionTree_ =
        _.ActionTree, (fun newValue instance -> { instance with ActionTree = newValue })

and LocalIconActionTree =
    | EvaluateIcon of DrawnIconPrism * next : LocalIconActionTree
    | EvaluateIf of DrawnIconPrism * falseBranch : LocalIconActionTree * trueBranch : LocalIconActionTree
    | PickupNewIcon of IconType * next : LocalIconActionTree
    | PickupIcon of DrawnIconPrism * next : LocalIconActionTree
    | PickupNumber of UnderlyingNumberDataType * next : LocalIconActionTree
    | PickupExecutionStateParameter of parameterIndex : int * next : LocalIconActionTree
    | PlacePickup of MovableObjectTarget * next : LocalIconActionTree
    | CancelPickup of next : LocalIconActionTree
    | RemoveIcon of index : int * next : LocalIconActionTree
    | RemoveIconParameter of target : DrawnIconPrism * position : int * next : LocalIconActionTree
    | Blank

and CustomIconName = string
and CustomIcons = Map<CustomIconName, CustomIcon>
and CustomIconPrism = Prism<CustomIcons, CustomIcon>

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

let private removeIcon targetIndex state =
    state
    |> listRemoveIndex targetIndex ^% ExecutionState.LocalIcons_

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

and applyLocalAction customIcons action =
    match action with
    | EvaluateIcon (iconPrism, _) ->
        evaluateIcon customIcons iconPrism
    | EvaluateIf (ifPrism,_ , _) ->
        evaluateIf customIcons ifPrism
    | PickupNewIcon (iconType, _) ->
        (NewIcon iconType) ^= ExecutionState.HeldObject_
    | PickupIcon (iconPrism, _)->
        (ExistingIcon iconPrism) ^= ExecutionState.HeldObject_
    | PickupNumber (parameter, _) ->
        (Number parameter) ^= ExecutionState.HeldObject_
    | PickupExecutionStateParameter (parameterIndex, _) ->
        fun state ->
            let parameterOptic = ExecutionState.Parameters_ >-> List.pos_ parameterIndex
            let parameter = Option.get (state ^. parameterOptic) // Crashes on invalid index
            state |> (Number parameter) ^= ExecutionState.HeldObject_
    | CancelPickup _ ->
        NoObject ^= ExecutionState.HeldObject_
    | PlacePickup (target, _) ->
        placePickup target
    | RemoveIcon (iconIndex, _) ->
        removeIcon iconIndex
    | RemoveIconParameter (targetPrism ,position, _) ->
        setParameterAtPosition targetPrism position Trap
    | Blank -> id

and applyLocalActions customIcons (actionTree : LocalIconActionTree) state =
    let stateWithAppliedHeadAction = applyLocalAction customIcons actionTree state
    let boundApplyLocalActions next =
        applyLocalActions customIcons next stateWithAppliedHeadAction
    match actionTree with
    | EvaluateIcon (_, next) -> boundApplyLocalActions next
    | EvaluateIf (_ , falseBranch, trueBranch) ->
        let nextBranch = if stateWithAppliedHeadAction.CurrentBranchChoices.Head then trueBranch else falseBranch
        boundApplyLocalActions nextBranch
    | PickupNewIcon (_, next) -> boundApplyLocalActions next
    | PickupIcon (_, next)-> boundApplyLocalActions next
    | PickupExecutionStateParameter (_, next) -> boundApplyLocalActions next
    | PickupNumber (_, next) -> boundApplyLocalActions next
    | CancelPickup next -> boundApplyLocalActions next
    | PlacePickup (_, next) -> boundApplyLocalActions next
    | RemoveIcon (_, next) -> boundApplyLocalActions next
    | RemoveIconParameter (_, _, next) -> boundApplyLocalActions next
    | Blank -> state

and buildExecutionStateForCustomIcon customIcons (customIconOptic : CustomIconPrism) parameters =
    let baseExecutionState = baseExecutionState parameters
    let actionTree=
        let optic = customIconOptic >?> CustomIcon.LocalActionTree_
        customIcons ^. optic |> Option.get
    applyLocalActions customIcons actionTree baseExecutionState

let appendNewActionToTree newAction choicesList (actionTree : LocalIconActionTree) =
    let rec appendNewActionToTree' newAction choicesList actionTree =
        let boundAppendNewActionToTree = appendNewActionToTree' newAction choicesList
        match actionTree with
        | EvaluateIcon (iconPrism, next) ->
            EvaluateIcon(iconPrism, appendNewActionToTree' newAction choicesList next)
        | EvaluateIf (ifPrism, falseBranch, trueBranch) ->
            match choicesList with
            | false :: restChoices ->
                EvaluateIf(ifPrism, appendNewActionToTree' newAction restChoices falseBranch, trueBranch)
            | true :: restChoices ->
                EvaluateIf(ifPrism, falseBranch, appendNewActionToTree' newAction restChoices trueBranch)
            | [] -> failwith "Missing choice"
        | PickupNewIcon (iconType, next) ->
            PickupNewIcon(iconType, boundAppendNewActionToTree next)
        | PickupIcon (iconPrism, next) ->
            PickupIcon(iconPrism, boundAppendNewActionToTree next)
        | PickupExecutionStateParameter (parameterIndex, next) ->
            PickupExecutionStateParameter(parameterIndex, boundAppendNewActionToTree next)
        | PickupNumber (number, next) ->
            PickupNumber(number, boundAppendNewActionToTree next)
        | CancelPickup next ->
            CancelPickup(boundAppendNewActionToTree next)
        | PlacePickup (target, next) ->
            PlacePickup(target, boundAppendNewActionToTree next)
        | RemoveIcon (iconIndex, next) ->
            RemoveIcon(iconIndex, boundAppendNewActionToTree next)
        | RemoveIconParameter (targetPrism, position, next) ->
            RemoveIconParameter(targetPrism, position, boundAppendNewActionToTree next)
        | Blank -> newAction

    appendNewActionToTree' newAction (List.rev choicesList) actionTree