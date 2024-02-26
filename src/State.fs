module PygmalionReimplementation.State

open System

open PygmalionReimplementation.Eval
open PygmalionReimplementation.Icons
open PygmalionReimplementation.Utils

type MovableObject =
    | NoObject
    | ExistingIcon of IconID
    | NewIcon of IconType
    | Parameter of IconInstructionParameter

type MovableObjectTarget =
    | Position of x : int * y : int
    | IconParameter of target : IconID * position : int

type TabState =
    { Name : string
      MasterCustomIconName : string
      MasterCustomIconParameters : Lazy<int> list
      IconResultData : IconResultsTable }

type State =
    { HeldObject : MovableObject
      CustomIcons : CustomIcons
      ConstantSpawnerText : string
      CustomIconCreatorName : string
      CustomIconCreatorParameterCount : string
      CurrentTabIndex : int
      Tabs : TabState list }


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
    | ChangeCustomIconCreatorName of name : string
    | ChangeCustomIconCreatorParameterCount of count : string
    | SwitchToTab of index : int
    | RemoveTab of index : int
    | CreateCustomIcon of Name : string * ParameterCount : int
    | EditCustomIcon of Name : string
    | NotImplemented of message : string

let getCurrentTab (state : State) =
    state.Tabs.[state.CurrentTabIndex]

let getMasterCustomIconName (state : State) =
    getCurrentTab state
    |> fun tab -> tab.MasterCustomIconName

let getMasterCustomIcon (state : State) =
    getMasterCustomIconName state
    |> fun name -> state.CustomIcons.[name]

let getMasterCustomIconParameters (state : State) =
    getCurrentTab state
    |> fun tab -> tab.MasterCustomIconParameters

let getIconTableFromState (state : State) =
    getCurrentTab state
    |> fun tab ->
        state.CustomIcons.[tab.MasterCustomIconName].SavedIcons

let getIconFromState (state : State) (id : IconID) =
    getIconTableFromState state |> Map.find id

let getIconResultsTableFromState (state : State) =
    getCurrentTab state
    |> fun tab -> tab.IconResultData

let getIconResultFromState (state : State) (id : IconID) =
    getIconResultsTableFromState state
    |> Map.tryFind id

let mergeIconResults (firstResults : IconResultsTable) (secondResults : IconResultsTable) =
    Map.fold
        (fun acc key value ->
            match Map.tryFind key acc with
            | Some _ ->
                acc
            | None ->
                acc.Add(key, value))
        firstResults
        secondResults

let stateWithNewIconResults (results : IconResultsTable) (state : State) =
    let iconResultsTable = getIconResultsTableFromState state
    { state with
        Tabs =
            state.Tabs
            |> List.mapi (fun i tab ->
                if i = state.CurrentTabIndex then
                    { tab with IconResultData = results }
                else
                    tab ) }

let stateWithMergedIconResults (newResults : IconResultsTable) (state : State) =
    let oldResults = getIconResultsTableFromState state
    stateWithNewIconResults (mergeIconResults oldResults newResults) state

let private stateWithNewIconTable (state : State) (newIconTable : IconTable) =
    let customIconName = getMasterCustomIconName state
    { state with
        CustomIcons =
            state.CustomIcons
            |> Map.add
                customIconName
                { state.CustomIcons[customIconName] with SavedIcons = newIconTable } }

let private stateWithNewEntryPoint (id : IconID option) (state : State) =
    let customIconName = getMasterCustomIconName state
    { state with
        CustomIcons =
            state.CustomIcons
            |> Map.add
                customIconName
                { state.CustomIcons[customIconName] with EntryPointIcon = id } }

let private stateWithNewIcon (state : State) (id : IconID) (newIcon : DrawnIcon) =
    let currentIconHasEntryPoint = getMasterCustomIcon state |> fun icon -> icon.EntryPointIcon <> None
    let newState = stateWithNewIconTable state (getIconTableFromState state |> Map.add id newIcon)
    // First placed icon is automatically the entry point
    if not currentIconHasEntryPoint then
        newState |> stateWithNewEntryPoint (Some(id))
    else
        newState

let private stateReplaceParameter
    (state : State)
    (targetID : IconID)
    (position : int)
    (newParameter : IconInstructionParameter) =
    let icon = getIconFromState state targetID
    { icon with IconInstruction = replaceParameter position newParameter icon.IconInstruction }
    |> stateWithNewIcon state targetID

let private evalIconFromState (state : State) (id : IconID) =
    let context =
        { CustomIcons = state.CustomIcons
          ExecutingCustomIconName = (getCurrentTab state).MasterCustomIconName
          CurrentIconID = Some id
          Parameters = getMasterCustomIconParameters state }
    stateWithMergedIconResults (eval context id) state

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
            IconInstruction = removeIconReferencesFromInstruction icon.IconInstruction }
    table
    |> Map.map (fun _ icon -> removeIconReferencesFromIcon icon)

let removeTabFromState (state : State) (index : int) =
    {state with
        Tabs = listRemoveIndex index state.Tabs
        CurrentTabIndex = state.CurrentTabIndex - (if index < state.CurrentTabIndex then 1 else 0) }

let isCurrentEntryPoint (id : IconID) (state : State) =
    getMasterCustomIcon state
    |> fun icon -> icon.EntryPointIcon = Some id


let randomNameGenerator len =
    let charSet = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
    Random ()
    |> fun rand ->
        [| for i in 1 .. len -> charSet.[rand.Next(charSet.Length)] |]
    |> String

let unevaluateAllMatchingTabs (masterIconName : string) (state : State) =
    { state with
        Tabs =
            state.Tabs
            |> List.map (fun tab ->
                if tab.MasterCustomIconName = masterIconName then
                    { tab with IconResultData = Map.empty }
                else
                    tab ) }

let addNewTab (tab : TabState) (state : State) =
    { state with
        Tabs = state.Tabs @ [ tab ] }

let dummyCustomIconName = randomNameGenerator 32
let private dummyCustomIcon : CustomIconType =
    { ParameterCount = 0
      SavedIcons = Map.empty
      EntryPointIcon = None }

let private initialCustomIcons : CustomIcons =
    Map.empty |> Map.add dummyCustomIconName dummyCustomIcon

let mainTabName = "Main"

let initalTabState =
    { Name = mainTabName
      MasterCustomIconName = dummyCustomIconName
      MasterCustomIconParameters = List.empty
      IconResultData = Map.empty }

let init () : State =
    { HeldObject = NoObject
      CustomIcons = initialCustomIcons
      ConstantSpawnerText = String.Empty
      CustomIconCreatorName = String.Empty
      CustomIconCreatorParameterCount = String.Empty
      CurrentTabIndex = 0
      Tabs = [ initalTabState ] }

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
        try
            evalIconFromState state id
        with TrapException(context, partialResults) ->
            let newTab =
                { Name = context.ExecutingCustomIconName
                  MasterCustomIconName = context.ExecutingCustomIconName
                  MasterCustomIconParameters = context.Parameters
                  IconResultData = partialResults }
            state
            |> addNewTab newTab
            |> fun state -> { state with CurrentTabIndex = state.Tabs.Length - 1 }

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
        |> unevaluateAllMatchingTabs (getMasterCustomIconName state)
        |> fun newState ->
            if isCurrentEntryPoint id newState then
                newState |> stateWithNewEntryPoint None
            else
                newState
    | RemoveIconParameter (targetID, position) ->
        getIconFromState state targetID
        |> fun icon ->
            { icon with IconInstruction = replaceParameter position Trap icon.IconInstruction }
        |> stateWithNewIcon state targetID
    | ChangeConstantSpawnerText text ->
        { state with ConstantSpawnerText = text }
    | ChangeCustomIconCreatorName name ->
        { state with CustomIconCreatorName = name }
    | ChangeCustomIconCreatorParameterCount count ->
        { state with CustomIconCreatorParameterCount = count }
    | SwitchToTab index ->
        match index with
        | i when i >= 0 && i < state.Tabs.Length ->
            { state with CurrentTabIndex = index }
            |> removeHeldObject
        | _ ->
            state
    | EditCustomIcon name ->
        let newTab =
            { Name = name
              MasterCustomIconName = name
              MasterCustomIconParameters = List.empty
              IconResultData = Map.empty }

        { state with Tabs = state.Tabs @ [ newTab ] }

    | CreateCustomIcon(name, parameterCount) ->
        match Map.tryFind name state.CustomIcons with
        | Some _ ->
            state
        | None ->
            let newCustomIcon =
                { ParameterCount = parameterCount
                  SavedIcons = Map.empty
                  EntryPointIcon = None }
            { state with
                CustomIcons =
                    state.CustomIcons
                    |> Map.add name newCustomIcon }
    | RemoveTab index ->
        removeTabFromState state index
    | NotImplemented message ->
        printf "%s" message
        state
