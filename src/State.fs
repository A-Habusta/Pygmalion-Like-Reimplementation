module PygmalionReimplementation.State

open System

open PygmalionReimplementation.Eval
open PygmalionReimplementation.Icons

type MovableObject =
    | NoObject
    | ExistingIcon of IconID
    | NewIcon of IconType
    | Parameter of IconInstructionParameter

type MovableObjectTarget =
    | Position of x : int * y : int
    | IconParameter of target : IconID * position : int

type State =
    { HeldObject : MovableObject
      CustomIcons : CustomIcons
      MasterCustomIconName : string
      MasterCustomIconParameters : Lazy<int> list
      ConstantSpawnerText : string }

type Message =
    | EvaluateIcon of IconID
    | PickupNewIcon of IconType
    | PickupIcon of IconID
    | PickupIconParameter of IconInstructionParameter
    | PlacePickup of MovableObjectTarget
    | CancelPickup
    | RemoveIcon of IconID
    | RemoveIconParameter of target : IconID * position : int
    | ChangeConstantSpawnerText of text : string
    | NotImplemented of message : string

let getIconTableFromState (state : State) =
    state.CustomIcons[state.MasterCustomIconName].SavedIcons

let getIconFromState (state : State) (id : IconID) =
    getIconTableFromState state |> Map.find id

let private stateWithNewIconTable (state : State) (newIconTable : IconTable) =
    { state with
        CustomIcons =
            state.CustomIcons
            |> Map.add
                state.MasterCustomIconName
                { state.CustomIcons[state.MasterCustomIconName] with SavedIcons = newIconTable } }

let private stateWithNewIcon (state : State) (id : IconID) (newIcon : DrawnIcon) =
    stateWithNewIconTable state (getIconTableFromState state |> Map.add id newIcon)

let private stateReplaceParameter
    (state : State)
    (targetID : IconID)
    (position : int)
    (newParameter : IconInstructionParameter) =
    let icon = getIconFromState state targetID
    { icon with IconInstruction = replaceParameter position newParameter icon.IconInstruction }
    |> stateWithNewIcon state targetID

let private evalIconFromState (state : State) (id : IconID) =
    let icon = getIconFromState state id
    let context =
        { CustomIcons = state.CustomIcons
          ExecutingCustomIcon = state.CustomIcons[state.MasterCustomIconName]
          CurrentIconID = id
          Parameters = state.MasterCustomIconParameters }
    let newIcon = {icon with Result = Some (eval context icon.IconInstruction) }
    stateWithNewIcon state id newIcon

let private removeIconReferencesFromTable (targetID : IconID) (table : IconTable) : IconTable =
    let removeIconReferencesFromIcon (icon : DrawnIcon) =
        let removeIconReferencesFromInstruction (instruction : IconInstruction) =
            let removeIconReferencesFromParameter (parameter : IconInstructionParameter) =
                match parameter with
                | LocalIconInstructionReference id when id = targetID ->
                    Trap
                | _ ->
                    parameter
            transformInstructionParameters (List.map removeIconReferencesFromParameter) instruction
        { icon with
            IconInstruction = removeIconReferencesFromInstruction icon.IconInstruction
            Result = None } // Automatically unEvaluates icons
    table
    |> Map.map (fun _ icon -> removeIconReferencesFromIcon icon)


let randomNameGenerator len =
    let random = Random ()
    let charSet = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
    let charList = [| for i in 1 .. len -> charSet.[random.Next(charSet.Length)] |]
    String charList

let dummyCustomIconName = randomNameGenerator 64
let private dummyCustomIcon : CustomIconType =
    { ParameterCount = 0
      SavedIcons = Map.empty
      EntryPointIcon = newIconID () }

let private initialCustomIcons : CustomIcons =
    Map.empty |> Map.add dummyCustomIconName dummyCustomIcon

let init () : State =
    { HeldObject = NoObject
      CustomIcons = initialCustomIcons
      MasterCustomIconName = dummyCustomIconName
      MasterCustomIconParameters = []
      ConstantSpawnerText = String.Empty }

let update (message : Message) (state : State) : State =
    let removeHeldObject (state : State) =
        {state with HeldObject = NoObject}
    let placePickup (targetLocation : MovableObjectTarget) =
        match state.HeldObject with
        | NoObject ->
            None
        | NewIcon newIconType ->
            match targetLocation with
            | Position (x, y) ->
                createDrawnIcon x y newIconType
                |> stateWithNewIcon state (newIconID ())
                |> Some
            | _ ->
                None
        | ExistingIcon iconID ->
            match targetLocation with
            | Position (x, y) ->
                getIconFromState state iconID
                |> fun icon -> { icon with X = x; Y = y }
                |> stateWithNewIcon state iconID
                |> Some
            | _ ->
                None
        | Parameter parameter ->
            match targetLocation with
            | IconParameter (targetID, position) ->
                getIconFromState state targetID
                |> fun icon ->
                    { icon with IconInstruction = replaceParameter position parameter icon.IconInstruction }
                |> stateWithNewIcon state targetID
                |> Some
            | _ ->
                None
    match message with
    | EvaluateIcon id ->
        // TODO: Add error handling
        try
            evalIconFromState state id
        with TrapException e ->
            printf "Trap!"
            state
    | PickupNewIcon iconType ->
        {state with HeldObject = NewIcon iconType}
    | PickupIcon id ->
        {state with HeldObject = ExistingIcon id}
    | PickupIconParameter parameter ->
        {state with HeldObject = Parameter parameter}
    | CancelPickup ->
        removeHeldObject state
    | PlacePickup targetLocation ->
        match placePickup targetLocation with
        | Some newState ->
            removeHeldObject newState
        | None -> state
    | RemoveIcon id ->
        getIconTableFromState state
        |> Map.remove id
        |> removeIconReferencesFromTable id
        |> stateWithNewIconTable state
    | RemoveIconParameter (targetID, position) ->
        getIconFromState state targetID
        |> fun icon ->
            { icon with IconInstruction = replaceParameter position Trap icon.IconInstruction }
        |> stateWithNewIcon state targetID
    | ChangeConstantSpawnerText text ->
        { state with ConstantSpawnerText = text }
    | NotImplemented message ->
        printf "%s" message
        state
