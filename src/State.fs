module PygmalionReimplementation.State

open System

open PygmalionReimplementation.Eval
open PygmalionReimplementation.Icons
open PygmalionReimplementation.Utils

type MovableObject =
    | NoObject
    | ExistingIcon of IconID
    | NewIcon of IconType
    | Parameter of IconOperationParameter

type MovableObjectTarget =
    | Position of x : int * y : int
    | IconParameter of target : IconID * position : int

type TabState =
    { Name : string
      MasterCustomOperationName : string
      MasterCustomOperationParameters : int list
      IconResultData : IconResultsTable }

type State =
    { HeldObject : MovableObject
      CustomOperations : CustomOperations
      ConstantSpawnerText : string
      CustomOperationCreatorName : string
      CustomOperationCreatorParameterCount : string
      CurrentTabIndex : int
      Tabs : TabState list }


type Message =
    | EvaluateIcon of IconID
    | PickupNewIcon of IconType
    | PickupIcon of IconID
    | PickupIconParameter of IconOperationParameter
    | PlacePickup of MovableObjectTarget
    | CancelPickup
    | RemoveIcon of IconID
    | RemoveIconParameter of target : IconID * position : int
    | ChangeConstantSpawnerText of text : string
    | ChangeCustomOperationCreatorName of name : string
    | ChangeCustomOperationCreatorParameterCount of count : string
    | SwitchToTab of index : int
    | RemoveTab of index : int
    | CreateCustomOperation of Name : string * ParameterCount : int
    | EditCustomOperation of Name : string
    | NotImplemented of message : string

let getCurrentTab (state : State) =
    state.Tabs.[state.CurrentTabIndex]

let getMasterCustomOperationName (state : State) =
    getCurrentTab state
    |> fun tab -> tab.MasterCustomOperationName

let getMasterCustomOperation (state : State) =
    getMasterCustomOperationName state
    |> fun name -> state.CustomOperations.[name]

let getMasterCustomOperationParameters (state : State) =
    getCurrentTab state
    |> fun tab -> tab.MasterCustomOperationParameters

let getIconTableFromState (state : State) =
    getCurrentTab state
    |> fun tab ->
        state.CustomOperations.[tab.MasterCustomOperationName].SavedIcons

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
    let customOperationName = getMasterCustomOperationName state
    { state with
        CustomOperations =
            state.CustomOperations
            |> Map.add
                customOperationName
                { state.CustomOperations[customOperationName] with SavedIcons = newIconTable } }

let private stateWithNewEntryPoint (id : IconID option) (state : State) =
    let customOperationName = getMasterCustomOperationName state
    { state with
        CustomOperations =
            state.CustomOperations
            |> Map.add
                customOperationName
                { state.CustomOperations[customOperationName] with EntryPointIcon = id } }

let private stateWithNewCustomOperation (name : string) (icon : CustomOperation) (state : State) =
    { state with
        CustomOperations =
            state.CustomOperations
            |> Map.add name icon }

let private stateWithNewIcon (state : State) (id : IconID) (newIcon : Icon) =
    let currentIconHasEntryPoint = getMasterCustomOperation state |> fun icon -> icon.EntryPointIcon <> None
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
    (newParameter : IconOperationParameter) =
    let icon = getIconFromState state targetID
    { icon with Operation = replaceParameter position newParameter icon.Operation }
    |> stateWithNewIcon state targetID

let private evalIconFromState (state : State) (id : IconID) =
    let context =
        { CustomOperations = state.CustomOperations
          ExecutingCustomOperationName = (getCurrentTab state).MasterCustomOperationName
          CurrentIconID = Some id
          Parameters = getMasterCustomOperationParameters state
          RecursionDepth = 0 }
    stateWithMergedIconResults (eval context id) state

let private removeIconReferencesFromTable (targetID : IconID) (table : IconTable) : IconTable =
    let removeIconReferencesFromIcon (icon : Icon) =
        let removeIconReferencesFromInstruction (instruction : IconOperation) =
            let removeIconReferencesFromParameter (parameter : IconOperationParameter) =
                match parameter with
                | LocalIconReference id when id = targetID ->
                    Trap
                | _ ->
                    parameter
            transformOperationParameters (List.map removeIconReferencesFromParameter) instruction
        { icon with
            Operation = removeIconReferencesFromInstruction icon.Operation }
    table
    |> Map.map (fun _ icon -> removeIconReferencesFromIcon icon)

let removeTabFromState (state : State) (index : int) =
    {state with
        Tabs = listRemoveIndex index state.Tabs
        CurrentTabIndex = state.CurrentTabIndex - (if index < state.CurrentTabIndex then 1 else 0) }

let removeCustomOperation (name : string) (state : State) =
//    let newCustomOperations = state.CustomOperations |> Map.remove name
//    let newTabs =
//        state.Tabs |>
//    { state with
//        CustomOperations = newCustomOperations
//        Tabs = newTabs
//        CurrentTabIndex = 0 }
    //TODO:
    state

let isCurrentEntryPoint (id : IconID) (state : State) =
    getMasterCustomOperation state
    |> fun icon -> icon.EntryPointIcon = Some id

let unevaluateAllMatchingTabs (masterIconName : string) (state : State) =
    { state with
        Tabs =
            state.Tabs
            |> List.map (fun tab ->
                if tab.MasterCustomOperationName = masterIconName then
                    { tab with IconResultData = Map.empty }
                else
                    tab ) }

let private addCustomIfOperations trueName falseName (state : State) =
    let parentParamCount = getMasterCustomOperation state |> fun icon -> icon.ParameterCount

    let newTrueCustomOperation =
        { ParameterCount = parentParamCount
          SavedIcons = Map.empty
          EntryPointIcon = None }

    let newFalseCustomOperation =
        { ParameterCount = parentParamCount
          SavedIcons = Map.empty
          EntryPointIcon = None }

    state
    |> stateWithNewCustomOperation trueName newTrueCustomOperation
    |> stateWithNewCustomOperation falseName newFalseCustomOperation

let private removeCustomIfIcons ifID (state : State) =
    let trueName, falseName = getCustomIfIconNames (getMasterCustomOperationName state) ifID
    let newCustomOperations =
        state.CustomOperations
        |> Map.remove trueName
        |> Map.remove falseName

    let newTabs =
        state.Tabs
        |> List.filter
            (fun tab -> tab.MasterCustomOperationName <> trueName && tab.MasterCustomOperationName <> falseName)

    { state with
        CustomOperations = newCustomOperations
        Tabs = newTabs
        CurrentTabIndex = 0 }

let private createIfIcon (ifID : IconID) (state : State) =
    let trueName, falseName = getCustomIfIconNames (getMasterCustomOperationName state) ifID
    addCustomIfOperations trueName falseName state

let private removeIcon (id : IconID) (state : State) =
    let iconType = getIconFromState state id |> fun icon -> icon.IconType
    let stateWithRemovedIcon =
        getIconTableFromState state
        |> Map.remove id
        |> removeIconReferencesFromTable id
        |> stateWithNewIconTable state
        |> unevaluateAllMatchingTabs (getMasterCustomOperationName state)
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

let dummyCustomOperationName = "_main"

let private dummyCustomOperation : CustomOperation =
    { ParameterCount = 0
      SavedIcons = Map.empty
      EntryPointIcon = None }

let private initialCustomOperations : CustomOperations =
    Map.empty |> Map.add dummyCustomOperationName dummyCustomOperation

let mainTabName = "Main"

let initalTabState =
    { Name = mainTabName
      MasterCustomOperationName = dummyCustomOperationName
      MasterCustomOperationParameters = List.empty
      IconResultData = Map.empty }

let init () : State =
    { HeldObject = NoObject
      CustomOperations = initialCustomOperations
      ConstantSpawnerText = String.Empty
      CustomOperationCreatorName = String.Empty
      CustomOperationCreatorParameterCount = String.Empty
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
                createIcon x y newIconType
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
                    { icon with Operation = replaceParameter position parameter icon.Operation }
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
                    { Name = context.ExecutingCustomOperationName
                      MasterCustomOperationName = context.ExecutingCustomOperationName
                      MasterCustomOperationParameters = context.Parameters
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
            { icon with Operation = replaceParameter position Trap icon.Operation }
        |> stateWithNewIcon state targetID
    | ChangeConstantSpawnerText text ->
        { state with ConstantSpawnerText = text }
    | ChangeCustomOperationCreatorName name ->
        if CustomOperationNameContainsInvalidCharacter name then
            state
        else
        { state with CustomOperationCreatorName = name }
    | ChangeCustomOperationCreatorParameterCount count ->
        { state with CustomOperationCreatorParameterCount = count }
    | SwitchToTab index ->
        switchToTab index state
    | EditCustomOperation name ->
        match Map.tryFind name state.CustomOperations with
        | Some customOperation ->
            let newTab =
                { Name = name
                  MasterCustomOperationName = name
                  MasterCustomOperationParameters = List.init customOperation.ParameterCount (fun _ -> 0)
                  IconResultData = Map.empty }
            state
            |> addNewTab newTab
            |> switchToLastTab
        | None ->
            state

    | CreateCustomOperation(name, parameterCount) ->
        match Map.tryFind name state.CustomOperations with
        | Some _ ->
            state
        | None ->
            if CustomOperationNameContainsInvalidCharacter name then
                state
            else
                let newCustomOperation =
                    { ParameterCount = parameterCount
                      SavedIcons = Map.empty
                      EntryPointIcon = None }
                state
                |> stateWithNewCustomOperation name newCustomOperation

    | RemoveTab index ->
        removeTabFromState state index
    | NotImplemented message ->
        printf "%s" message
        state
