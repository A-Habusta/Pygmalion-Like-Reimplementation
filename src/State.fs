module PygmalionReimplementation.State

open System
open Browser
open Elmish
open Elmish.React
open Feliz

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
      MasterCustomIconParameters : Lazy<int> list }

type Message =
    | EvaluateIcon of IconID
    | CreateIcon of IconType
    | PickupIcon of IconID
    | PickupIconParameter of IconInstructionParameter
    | PlacePickup of MovableObjectTarget
    | CancelPickup
    | RemoveIcon of IconID
    | RemoveIconParameter of target : IconID * position : int
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

let removeIconReferences (currentIcon : DrawnIcon) (targetID : IconID) : DrawnIcon =
    let removeIconReferencesFromInstruction (parameters : IconInstructionParameter list) : IconInstructionParameter list =
        parameters
            |> List.map (fun param ->
                match param with
                | LocalIconInstructionReference id when id = targetID -> Trap
                | icon -> icon )

    { currentIcon with
        IconInstruction = transformInstructionParameters removeIconReferencesFromInstruction currentIcon.IconInstruction
        Result = None }


let dummyCustomIconName = "dummy"
let dummyCustomIcon : CustomIconType =
    { ParameterCount = 0
      SavedIcons = Map.empty
      EntryPointIcon = newIconID () }

let initialCustomIcons : CustomIcons =
    Map.empty |> Map.add dummyCustomIconName dummyCustomIcon

let init () : State =
    { HeldObject = NoObject
      CustomIcons = initialCustomIcons
      MasterCustomIconName = dummyCustomIconName
      MasterCustomIconParameters = [] }

let update (message : Message) (state : State) : State =
    match message with
    | EvaluateIcon id ->
        // TODO: Add error handling
        try
            evalIconFromState state id
        with TrapException e ->
            printf "Trap!"
            state

    | CreateIcon iconType ->
        {state with HeldObject = NewIcon iconType}
    | PickupIcon id ->
        {state with HeldObject = ExistingIcon id}
    | PickupIconParameter parameter ->
        {state with HeldObject = Parameter parameter}
    | CancelPickup ->
        {state with HeldObject = NoObject}
    | PlacePickup targetLocation ->
        match state.HeldObject with
        | NoObject -> failwith "Tried to place object without holding any"
        | NewIcon newIconType ->
            match targetLocation with
            | Position (x, y) ->
                createDrawnIcon x y newIconType
                |> stateWithNewIcon state (newIconID ())
            | _ -> state
        | ExistingIcon iconID ->
            match targetLocation with
            | Position (x, y) ->
                let icon = getIconFromState state iconID
                let newIcon = { icon with X = x; Y = y }
                stateWithNewIcon state iconID newIcon
            | _ -> state
        | Parameter parameter ->
            match targetLocation with
            | IconParameter (targetID, position) ->
                let icon = getIconFromState state targetID
                let newIcon =
                    { icon with IconInstruction = replaceParameter position parameter icon.IconInstruction }
                stateWithNewIcon state targetID newIcon
            | _ -> state
    | RemoveIcon id ->
        getIconTableFromState state
        |> Map.remove id
        |> Map.map (fun _ icon -> removeIconReferences icon id)
        |> stateWithNewIconTable state
    | RemoveIconParameter (targetID, position) ->
        let icon = getIconFromState state targetID
        let newIcon =
            { icon with IconInstruction = replaceParameter position Trap icon.IconInstruction }
        stateWithNewIcon state targetID newIcon
    | NotImplemented message ->
        printf "%s" message
        state
