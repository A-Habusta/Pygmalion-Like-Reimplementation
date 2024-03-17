module PygmalionReimplementation.State

open Aether
open Aether.Operators

open PygmalionReimplementation.SimpleEval
open PygmalionReimplementation.Icons
open PygmalionReimplementation.Utils

type SingleTabState =
    { TabName : string
      TabCustomIconPrism : CustomIconPrism
      TabParameters : UnderlyingNumberDataType list }

    static member TabName_ =
        _.TabName, (fun value a -> { a with TabName = value })
    static member TabCustomIconPrism_ =
        _.TabCustomIconPrism, (fun value a -> { a with TabCustomIconPrism = value })
    static member TabParameters_ =
        _.TabParameters, (fun value a -> { a with TabParameters = value })

type Tabs = SingleTabState list
type TabPrism = Prism<Tabs, SingleTabState>

let initialTabName = "Main"

type InputState =
    { ConstantSpawnerText : string
      CustomIconCreatorName : string
      CustomIconCreatorParameterCount : string }

    static member ConstantSpawnerText_ =
        _.ConstantSpawnerText, (fun value a -> { a with ConstantSpawnerText = value })
    static member CustomIconCreatorName_ =
        _.CustomIconCreatorName, (fun value a -> { a with CustomIconCreatorName = value })
    static member CustomIconCreatorParameterCount_ =
        _.CustomIconCreatorParameterCount, (fun value a -> { a with CustomIconCreatorParameterCount = value })

type State =
    { CustomIcons : CustomIcons
      ExecutionState : ExecutionState
      Tabs : Tabs
      InputState : InputState }

    static member CustomIcons_ =
        _.CustomIcons, (fun value a -> { a with CustomIcons = value })
    static member ExecutionState_ =
        _.ExecutionState, (fun value a -> { a with ExecutionState = value })
    static member Tabs_ =
        _.Tabs, (fun value a -> { a with Tabs = value })
    static member InputState_ =
        _.InputState, (fun value a -> { a with InputState = value })
    static member CurrentTabPrism_ = State.Tabs_ >-> List.head_

type InputAction =
    | SetConstantSpawnerText of string
    | SetCustomIconCreatorName of string
    | SetCustomIconParameterCount of string
    | PressCustomIconCreatorButton
    | PressConstantSpawnerButton

type Action =
    | InputAction of InputAction
    | IconAction of ExecutionActionTree
    | CreateNewCustomIcon of string * int

let private createCustomIcon name (parameterCount : int) : CustomIcon =
    { Name = name
      ActionTree = defaultEnd
      ParameterCount = parameterCount }

let init () : State =
    let initialInputState =
        { ConstantSpawnerText = ""
          CustomIconCreatorName = ""
          CustomIconCreatorParameterCount = "" }
    let initialExecutionState = baseExecutionState List.empty

    let initialTab =
        { TabName = initialTabName
          TabCustomIconPrism = List.pos_ initialCustomIconIndex
          TabParameters = [] }
    let initialTabs = [initialTab]

    let initialCustomIcon = createCustomIcon initialCustomIconName 0
    let initialCustomIcons = [initialCustomIcon]

    { ExecutionState = initialExecutionState
      CustomIcons = initialCustomIcons
      Tabs = initialTabs
      InputState = initialInputState }

let createTab tabName tabCustomIconPrism parameters  =
    { TabName = tabName
      TabCustomIconPrism = tabCustomIconPrism
      TabParameters = parameters }

let createTabFromCustomIcon customIcons customIconPrism parameters =
    let customIcon = customIcons ^. customIconPrism |> Option.get
    let name = customIcon.Name
    createTab name customIconPrism parameters

let onTrap prism parameters executionState state =
    let tab = createTabFromCustomIcon state.CustomIcons prism parameters
    state |> cons tab ^% State.Tabs_ |> executionState ^= State.ExecutionState_

let executionContextEqual state (customIconPrism1, parameters1) (customIconPrism2, parameters2) =
        let customIconFromPrism prism = state.CustomIcons ^. prism |> Option.get
        let offendingCustomIcon = customIconFromPrism customIconPrism1
        let currentCustomIcon = customIconFromPrism customIconPrism2

        let customIconNamesEqual = offendingCustomIcon.Name = currentCustomIcon.Name
        let parametersEqual = parameters1 = parameters2

        customIconNamesEqual && parametersEqual

let switchToTopTab (state : State) : State =
    let tab = state ^. State.CurrentTabPrism_ |> Option.get
    try
        let executionState =
            buildExecutionStateForCustomIcon state.CustomIcons tab.TabCustomIconPrism tab.TabParameters
        state |> executionState ^= State.ExecutionState_
    with RecursionTrapException (offendingIconPrism, parameters, trapExecutionState) ->
        let trapInCurrentTab =
            executionContextEqual state (offendingIconPrism, parameters) (tab.TabCustomIconPrism, tab.TabParameters)
        if trapInCurrentTab then
            state |> trapExecutionState ^= State.ExecutionState_
        else
            onTrap offendingIconPrism parameters trapExecutionState state

let removeTopTab (state : State) : State =
    state |> List.tail ^% State.Tabs_ |> switchToTopTab

let appendNewTabToTop tab (state : State) : State =
    state |> cons tab ^% State.Tabs_ |> switchToTopTab

let wrapExecutionActionNode (action : ExecutionActionTree) : Action =
    IconAction action

let rec update (action : Action) state =
    let updateWithInputAction inputAction state =
        let appendReverse list1 list2 = List.append list2 list1
        match inputAction with
        | SetConstantSpawnerText text ->
            state |> text ^= (State.InputState_ >-> InputState.ConstantSpawnerText_)
        | SetCustomIconCreatorName name ->
            state |> name ^= (State.InputState_ >-> InputState.CustomIconCreatorName_)
        | SetCustomIconParameterCount count ->
            state |> count ^= (State.InputState_ >-> InputState.CustomIconCreatorParameterCount_)
        | PressCustomIconCreatorButton ->
            let customIconName = state.InputState.CustomIconCreatorName
            let parameterCountText = state.InputState.CustomIconCreatorParameterCount
            match (isNumber parameterCountText, isCustomIconNameValid customIconName) with
            | (false, _) -> state
            | (_, false) -> state
            | (true, true) ->
                let parameterCount = int parameterCountText
                let customIcon = createCustomIcon customIconName parameterCount
                state |> appendReverse [customIcon] ^% State.CustomIcons_
        | PressConstantSpawnerButton ->
            let constantText = state.InputState.ConstantSpawnerText
            match isNumber constantText with
            | false -> state
            | true ->
                let constant = stringToUnderlyingNumberDataType constantText
                let newAction =
                    PickupNumber constant
                    |> wrapSimpleExecutionAction
                    |> wrapExecutionActionNode
                update newAction state

    let applyIconAction iconAction state =
        let rec removeTopTabIfResultIsSome state =
            if state.ExecutionState.Result |> Option.isSome then
                removeTopTab state |> removeTopTabIfResultIsSome
            else state
        let currentCustomIconPrism =
            let optic = State.CurrentTabPrism_ >?> SingleTabState.TabCustomIconPrism_
            state ^. optic |> Option.get // We expect only valid prisms to be used

        let choices = state.ExecutionState.CurrentBranchChoices
        let actionTreePrism =
            State.CustomIcons_ >-> currentCustomIconPrism >?> CustomIcon.LocalActionTree_
        let stateWithAction =
            state |> appendNewActionToTree iconAction choices ^% actionTreePrism

        try
            stateWithAction
            |> applyExecutionActionNode stateWithAction.CustomIcons iconAction ^% State.ExecutionState_
            |> removeTopTabIfResultIsSome
        with RecursionTrapException (offendingCustomIcon, parameters, executionState) ->
            onTrap offendingCustomIcon parameters executionState stateWithAction
    let createNewCustomIcon name parameterCount state =
        let nameAlreadyExists =
            state.CustomIcons |> List.exists (fun icon -> icon.Name = name)
        if nameAlreadyExists then
            state
        else
            let customIcon = createCustomIcon name parameterCount
            // We have to use append instead of cons to preserve the indices
            state |> listAppend customIcon ^% State.CustomIcons_

    match action with
    | CreateNewCustomIcon (name, parameterCount) ->
        createNewCustomIcon name parameterCount state
    | InputAction inputAction ->
        updateWithInputAction inputAction state
    | IconAction iconAction ->
        applyIconAction iconAction state