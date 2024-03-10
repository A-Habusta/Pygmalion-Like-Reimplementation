module PygmalionReimplementation.Icons

open System
open Aether
open Aether.Operators

open PygmalionReimplementation.SimpleEval

type UnderlyingNumberDataType = int

type IconInstructionParameter = UnderlyingNumberDataType option

type IconInstruction =
    | Unary of operator : string * IconInstructionParameter
    | Binary of operator : string * IconInstructionParameter * IconInstructionParameter
    | If of IconInstructionParameter
    | CallCustomIcon of customIconName : string * IconInstructionParameter list

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

type IconType =
    | BaseUnaryIcon of operation : string
    | BaseBinaryIcon of operation : string
    | BaseIfIcon
    | CustomIcon of customIconName : string * parameterCount : int

type DrawnIcon =
    { X : int
      Y : int
      IconInstruction : IconInstruction
      Result : UnderlyingNumberDataType option }

    static member X_ =
        let view instance = instance.X
        let update newValue instance = { instance with X = newValue }
        (view, update)
    static member Y_ =
        let view instance = instance.Y
        let update newValue instance = { instance with Y = newValue }
        (view, update)
    static member IconInstruction_ =
        let view instance = instance.IconInstruction
        let update newValue instance = { instance with IconInstruction = newValue }
        (view, update)
    static member Result_ =
        let view instance = instance.Result
        let update newValue instance = { instance with Result = newValue }
        (view, update)

type DrawnIconIndex = int
type DrawnIcons = Map<DrawnIconIndex, DrawnIcon>
type DrawnIconPrism = Prism<DrawnIcons, DrawnIcon>

type MovableObject =
    | NoObject
    | ExistingIcon of DrawnIconPrism
    | NewIcon of IconType
    | Parameter of IconInstructionParameter

type MovableObjectTarget =
    | Position of x : int * y : int
    | IconParameter of target : DrawnIconPrism * position : int

type CustomIcon =
    { ParameterCount : int
      ActionBranches : LocalIconAction list list
      CurrentBranchLens : IconActionBranchLens }

    static member ParameterCount_ =
        let view instance = instance.ParameterCount
        let update value instance = { instance with ParameterCount = value }
        (view, update)

    static member CurrentBranchLens_ =
        let view instance = instance.CurrentBranchLens
        let update value instance = { instance with CurrentBranchLens = value }
        (view, update)

and LocalIconAction =
    | EvaluateIcon of DrawnIconPrism
    | EvaluateIf of DrawnIconPrism * falseBranch : IconActionBranchLens // True branch just continues
    | PickupNewIcon of IconType
    | PickupIcon of DrawnIconPrism
    | PickupIconParameter of IconInstructionParameter
    | PlacePickup of MovableObjectTarget
    | CancelPickup
    | RemoveIcon of DrawnIconPrism
    | RemoveIconParameter of target : DrawnIconPrism * position : int

and IconActionBranchLens = Lens<LocalIconAction list list, LocalIconAction list option>

type CustomIconName = string
type CustomIcons = Map<CustomIconName, CustomIcon>
type CustomIconPrism = Prism<CustomIcons, CustomIcon>

type LocalState =
    { HeldObject : MovableObject
      LocalIcons : DrawnIcons
      AvailableIndex : DrawnIconIndex }

    static member HeldObject_ =
        let view instance = instance.HeldObject
        let update newValue instance = { instance with HeldObject = newValue }
        (view, update)
    static member AvailableIndex_ =
        let view instance = instance.AvailableIndex
        let update newValue instance = { instance with AvailableIndex = newValue }
        (view, update)
    static member LocalIcons_ =
        let view instance = instance.LocalIcons
        let update newValue instance = { instance with LocalIcons = newValue }
        (view, update)

let private invalidCustomIconNameCharacters = "\t\n\r_"

let Trap : IconInstructionParameter = None

let transformInstructionParameters transform =
    transform ^% IconInstruction.Params_ // Optic.map

let replaceParameter position newParameter instruction =
    let transform =
        List.mapi (fun i originalParameter ->
            if i = position then newParameter
            else originalParameter )
    transformInstructionParameters transform instruction

let createEmptyIconInstruction (iconType : IconType) =
    match iconType with
    | BaseUnaryIcon op -> Unary(op, Trap)
    | BaseBinaryIcon op -> Binary(op, Trap, Trap)
    | BaseIfIcon -> If(Trap)
    | CustomIcon(customIconName, paramCount) -> CallCustomIcon(customIconName, List.init paramCount (fun _ -> Trap))

let createEmptyDrawnIcon x y instruction =
    { IconInstruction = instruction
      Result = None
      X = x
      Y = y }

let customIconNameContainsInvalidCharacter (name : string) =
    name |> Seq.exists (fun c -> invalidCustomIconNameCharacters.Contains(c))

let incrementAvailableIndex =
    (fun value -> value + 1) ^% LocalState.AvailableIndex_

let private setParameterAtPosition targetPrism position targetParameter =
    let fullTargetOptic =
        LocalState.LocalIcons_ >-> targetPrism >?> DrawnIcon.IconInstruction_
    (replaceParameter position targetParameter) ^% fullTargetOptic

let private placePickup target state =
    let createNewIconAt x y iconType =
        let newId = state.AvailableIndex
        let newDrawnIcon =
            createEmptyIconInstruction iconType |> createEmptyDrawnIcon x y
        state
        |> Map.add newId newDrawnIcon ^% LocalState.LocalIcons_
        |> incrementAvailableIndex

    let placeIconAt x y targetPrism =
        let fullIconPrism = LocalState.LocalIcons_ >-> targetPrism
        state
        |> x ^= (fullIconPrism >?> DrawnIcon.X_)
        |> y ^= (fullIconPrism >?> DrawnIcon.Y_)

    let heldObject = state ^. LocalState.HeldObject_
    match (target, heldObject) with
    | (Position (x, y), NewIcon iconType) ->
        createNewIconAt x y iconType
    | (Position (x, y), ExistingIcon iconPrism) ->
        placeIconAt x y iconPrism
    | (IconParameter (targetPrism, position), Parameter parameter) ->
        setParameterAtPosition targetPrism position parameter state
    | (_, _) -> state

let private removeIcon targetPrism state =
    state ^. (LocalState.LocalIcons_ >-> targetPrism)
    |> function
        | Some icon ->
            let iconIndex =
                state ^. LocalState.LocalIcons_
                |> Map.findKey(fun _ item -> item = icon)
            state
            |> Map.remove iconIndex ^% LocalState.LocalIcons_
        | None -> state

let applyLocalAction action =
    match action with
    | EvaluateIcon iconPrism ->
        id
    | EvaluateIf (ifPrism, falseBranchLens) ->
        id
    | PickupNewIcon iconType ->
        (NewIcon iconType) ^= LocalState.HeldObject_
    | PickupIcon iconPrism->
        (ExistingIcon iconPrism) ^= LocalState.HeldObject_
    | PickupIconParameter parameter ->
        (Parameter parameter) ^= LocalState.HeldObject_
    | CancelPickup ->
        NoObject ^= LocalState.HeldObject_
    | PlacePickup target ->
        placePickup target
    | RemoveIcon iconPrism->
        removeIcon iconPrism
    | RemoveIconParameter (targetPrism , position) ->
        setParameterAtPosition targetPrism position Trap