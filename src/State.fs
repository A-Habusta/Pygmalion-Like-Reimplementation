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
      MasterCustomIconParameters : int list
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

let private stateWithNewCustomIcon (name : string) (icon : CustomIconType) (state : State) =
    { state with
        CustomIcons =
            state.CustomIcons
            |> Map.add name icon }

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
          Parameters = getMasterCustomIconParameters state
          RecursionDepth = 0 }
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

let removeCustomIcon (name : string) (state : State) =
//    let newCustomIcons = state.CustomIcons |> Map.remove name
//    let newTabs =
//        state.Tabs |>
//    { state with
//        CustomIcons = newCustomIcons
//        Tabs = newTabs
//        CurrentTabIndex = 0 }
    //TODO:
    state

let isCurrentEntryPoint (id : IconID) (state : State) =
    getMasterCustomIcon state
    |> fun icon -> icon.EntryPointIcon = Some id

let unevaluateAllMatchingTabs (masterIconName : string) (state : State) =
    { state with
        Tabs =
            state.Tabs
            |> List.map (fun tab ->
                if tab.MasterCustomIconName = masterIconName then
                    { tab with IconResultData = Map.empty }
                else
                    tab ) }

let private addCustomIfIcons trueName falseName (state : State) =
    let parentParamCount = getMasterCustomIcon state |> fun icon -> icon.ParameterCount

    let newTrueCustomIcon =
        { ParameterCount = parentParamCount
          SavedIcons = Map.empty
          EntryPointIcon = None }

    let newFalseCustomIcon =
        { ParameterCount = parentParamCount
          SavedIcons = Map.empty
          EntryPointIcon = None }

    state
    |> stateWithNewCustomIcon trueName newTrueCustomIcon
    |> stateWithNewCustomIcon falseName newFalseCustomIcon

let private removeCustomIfIcons ifID (state : State) =
    let trueName, falseName = getCustomIfIconNames (getMasterCustomIconName state) ifID
    let newCustomIcons =
        state.CustomIcons
        |> Map.remove trueName
        |> Map.remove falseName

    let newTabs =
        state.Tabs
        |> List.filter
            (fun tab -> tab.MasterCustomIconName <> trueName && tab.MasterCustomIconName <> falseName)

    { state with
        CustomIcons = newCustomIcons
        Tabs = newTabs
        CurrentTabIndex = 0 }

let private createIfIcon (ifID : IconID) (state : State) =
    let trueName, falseName = getCustomIfIconNames (getMasterCustomIconName state) ifID
    addCustomIfIcons trueName falseName state

let private removeIcon (id : IconID) (state : State) =
    let iconType = getIconFromState state id |> fun icon -> icon.IconType
    let stateWithRemovedIcon =
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

    match iconType with
    | BaseIfIcon ->
        removeCustomIfIcons id stateWithRemovedIcon
    | _ ->
        stateWithRemovedIcon

let dummyCustomIconName = "_main"

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
    let addNewTab (tab : TabState) (state : State) =
        { state with
            Tabs = state.Tabs @ [ tab ] }
    let switchToTab (index : int) (state : State) =
        match index with
        | i when i >= 0 && i < state.Tabs.Length ->
            { state with CurrentTabIndex = index }
            |> removeHeldObject
        | _ ->
            state
    let switchToLastTab (state : State) =
        switchToTab (state.Tabs.Length - 1) state
    let placePickup (targetLocation : MovableObjectTarget) =
        match state.HeldObject with
        | NoObject ->
            None
        | NewIcon newIconType ->
            match targetLocation with
            | Position (x, y) ->
                let newID = newIconID ()
                createDrawnIcon x y newIconType
                |> stateWithNewIcon state newID
                |> if newIconType = BaseIfIcon then createIfIcon newID else id
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
            if context.RecursionDepth = 0 then
                let newIconResults = mergeIconResults (getIconResultsTableFromState state) partialResults
                stateWithNewIconResults newIconResults state
            else
                let newTab =
                    { Name = context.ExecutingCustomIconName
                      MasterCustomIconName = context.ExecutingCustomIconName
                      MasterCustomIconParameters = context.Parameters
                      IconResultData = partialResults }
                state
                |> addNewTab newTab
                |> switchToLastTab

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
        removeIcon id state
    | RemoveIconParameter (targetID, position) ->
        getIconFromState state targetID
        |> fun icon ->
            { icon with IconInstruction = replaceParameter position Trap icon.IconInstruction }
        |> stateWithNewIcon state targetID
    | ChangeConstantSpawnerText text ->
        { state with ConstantSpawnerText = text }
    | ChangeCustomIconCreatorName name ->
        if CustomIconNameContainsInvalidCharacter name then
            state
        else
        { state with CustomIconCreatorName = name }
    | ChangeCustomIconCreatorParameterCount count ->
        { state with CustomIconCreatorParameterCount = count }
    | SwitchToTab index ->
        switchToTab index state
    | EditCustomIcon name ->
        match Map.tryFind name state.CustomIcons with
        | Some customIcon ->
            let newTab =
                { Name = name
                  MasterCustomIconName = name
                  MasterCustomIconParameters = List.init customIcon.ParameterCount (fun _ -> 0)
                  IconResultData = Map.empty }
            state
            |> addNewTab newTab
            |> switchToLastTab
        | None ->
            state

    | CreateCustomIcon(name, parameterCount) ->
        match Map.tryFind name state.CustomIcons with
        | Some _ ->
            state
        | None ->
            if CustomIconNameContainsInvalidCharacter name then
                state
            else
                let newCustomIcon =
                    { ParameterCount = parameterCount
                      SavedIcons = Map.empty
                      EntryPointIcon = None }
                state
                |> stateWithNewCustomIcon name newCustomIcon

    | RemoveTab index ->
        removeTabFromState state index
    | NotImplemented message ->
        printf "%s" message
        state
