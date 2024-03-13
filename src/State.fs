module PygmalionReimplementation.State

open Aether
open Aether.Operators

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
let incrementTabIndex = (+) 1

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
    | CloseTopTab

let private createCustomIcon (parameterCount : int) : CustomIcon =
    { ActionTree = End
      ParameterCount = parameterCount }

let init () : State =
    let initialInputState =
        { ConstantSpawnerText = ""
          CustomIconCreatorName = ""
          CustomIconCreatorParameterCount = "" }
    let initialExecutionState = baseExecutionState List.empty

    let initialTab =
        { TabName = initialTabName
          TabCustomIconPrism = Map.key_ initialCustomIconName
          TabParameters = [] }
    let initialTabs = [initialTab]

    { ExecutionState = initialExecutionState
      CustomIcons = Map.empty
      Tabs = initialTabs
      InputState = initialInputState }

let switchToTopTab (state : State) : State =
    let tab = state ^. State.CurrentTabPrism_ |> Option.get
    let newExecutionState =
        buildExecutionStateForCustomIcon state.CustomIcons tab.TabCustomIconPrism tab.TabParameters
    state |> newExecutionState ^= State.ExecutionState_

let removeTopTab (state : State) : State =
    state |> List.tail ^% State.Tabs_ |> switchToTopTab

let appendNewTabToTop tab (state : State) : State =
    state |> cons tab ^% State.Tabs_ |> switchToTopTab

let createTab tabName tabCustomIconPrism parameters  =
    { TabName = tabName
      TabCustomIconPrism = tabCustomIconPrism
      TabParameters = parameters }

let isCustomIconNameValid name =
    isText name && not (customIconNameContainsInvalidCharacter name)

let wrapExecutionActionNode (action : ExecutionActionTree) : Action =
    IconAction action

let rec update (action : Action) =
    let updateWithInputAction inputAction =
        match inputAction with
        | SetConstantSpawnerText text ->
            text ^= (State.InputState_ >-> InputState.ConstantSpawnerText_)
        | SetCustomIconCreatorName name ->
            name ^= (State.InputState_ >-> InputState.CustomIconCreatorName_)
        | SetCustomIconParameterCount count ->
            count ^= (State.InputState_ >-> InputState.CustomIconCreatorParameterCount_)
        | PressCustomIconCreatorButton ->
            fun state ->
                let customIconName = state.InputState.CustomIconCreatorName
                let parameterCountText = state.InputState.CustomIconCreatorParameterCount
                match (isNumber parameterCountText, isCustomIconNameValid customIconName) with
                | (false, _) -> state
                | (_, false) -> state
                | (true, true) ->
                    let parameterCount = int parameterCountText
                    let customIcon = createCustomIcon parameterCount
                    state |> Map.add customIconName customIcon ^% State.CustomIcons_
        | PressConstantSpawnerButton ->
            fun state ->
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

    match action with
    | CloseTopTab ->
        removeTopTab
    | InputAction inputAction ->
        updateWithInputAction inputAction
    | IconAction iconAction ->
        fun state ->
            let customIcons = state.CustomIcons
            let currentCustomIconPrism =
                let optic = State.CurrentTabPrism_ >?> SingleTabState.TabCustomIconPrism_
                state ^. optic |> Option.get // We expect only valid prisms to be used

            let actionTreePrism = State.CustomIcons_ >-> currentCustomIconPrism >?> CustomIcon.LocalActionTree_
            let choices = state.ExecutionState.CurrentBranchChoices
            state
            |> applyExecutionActionNode customIcons iconAction ^% State.ExecutionState_
            |> appendNewActionToTree iconAction choices ^% actionTreePrism