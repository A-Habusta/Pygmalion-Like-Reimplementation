module PygmalionReimplementation.State

open Aether
open Aether.Operators

open PygmalionReimplementation.SimpleEval
open PygmalionReimplementation.Icons
open PygmalionReimplementation.Utils

type SingleTabState =
    { TabName : string
      TabCustomOperationPrism : CustomOperationPrism
      TabParameters : UnderlyingNumberDataType list }

    static member TabName_ =
        _.TabName, (fun value a -> { a with TabName = value })
    static member TabCustomOperationPrism_ =
        _.TabCustomOperationPrism, (fun value a -> { a with TabCustomOperationPrism = value })
    static member TabParameters_ =
        _.TabParameters, (fun value a -> { a with TabParameters = value })

type Tabs = SingleTabState list
type TabPrism = Prism<Tabs, SingleTabState>

let initialTabName = "Main"

type InputState =
    { ConstantSpawnerText : string
      CustomOperationCreatorName : string
      CustomOperationCreatorParameterCount : string }

    static member ConstantSpawnerText_ =
        _.ConstantSpawnerText, (fun value a -> { a with ConstantSpawnerText = value })
    static member CustomOperationCreatorName_ =
        _.CustomOperationCreatorName, (fun value a -> { a with CustomOperationCreatorName = value })
    static member CustomOperationCreatorParameterCount_ =
        _.CustomOperationCreatorParameterCount, (fun value a -> { a with CustomOperationCreatorParameterCount = value })

type State =
    { CustomOperations : CustomOperations
      ExecutionState : ExecutionState
      Tabs : Tabs
      InputState : InputState
      IntialPopupClosed : bool }

    static member CustomOperations_ =
        _.CustomOperations, (fun value a -> { a with CustomOperations = value })
    static member ExecutionState_ =
        _.ExecutionState, (fun value a -> { a with ExecutionState = value })
    static member Tabs_ =
        _.Tabs, (fun value a -> { a with Tabs = value })
    static member InputState_ =
        _.InputState, (fun value a -> { a with InputState = value })
    static member IntialPopupClosed_ =
        _.IntialPopupClosed, (fun value a -> { a with IntialPopupClosed = value })
    static member CurrentTabPrism_ = State.Tabs_ >-> List.head_


type InputAction =
    | SetConstantSpawnerText of string
    | SetCustomOperationCreatorName of string
    | SetCustomOperationParameterCount of string
    | PressCustomOperationCreatorButton
    | PressConstantSpawnerButton

type Action =
    | InputAction of InputAction
    | IconAction of ExecutionActionTree
    | CreateNewCustomOperation of string * int
    | CloseInitialPopup

let private createCustomOperation name (parameterCount : int) : CustomOperation =
    { Name = name
      ActionTree = defaultEnd
      ParameterCount = parameterCount }

let init () : State =
    let initialInputState =
        { ConstantSpawnerText = ""
          CustomOperationCreatorName = ""
          CustomOperationCreatorParameterCount = "" }
    let initialExecutionState = baseExecutionState List.empty

    let initialTab =
        { TabName = initialTabName
          TabCustomOperationPrism = List.pos_ initialCustomOperationIndex
          TabParameters = [] }
    let initialTabs = [initialTab]

    let initialCustomOperation = createCustomOperation initialCustomOperationName 0
    let initialCustomOperations = [initialCustomOperation]

    { ExecutionState = initialExecutionState
      CustomOperations = initialCustomOperations
      Tabs = initialTabs
      InputState = initialInputState
      IntialPopupClosed = false }

let createTab tabName tabCustomOperationPrism parameters  =
    { TabName = tabName
      TabCustomOperationPrism = tabCustomOperationPrism
      TabParameters = parameters }

let createTabFromCustomOperation customOperations customOperationPrism parameters =
    let customOperation = customOperations ^. customOperationPrism |> Option.get
    let name = customOperation.Name
    createTab name customOperationPrism parameters

let onTrap prism executionState state =
    let tab = createTabFromCustomOperation state.CustomOperations prism executionState.Parameters
    state |> cons tab ^% State.Tabs_ |> executionState ^= State.ExecutionState_

let executionContextEqual state (customOperationPrism1, parameters1) (customOperationPrism2, parameters2) =
        let customOperationFromPrism prism = state.CustomOperations ^. prism |> Option.get
        let offendingCustomOperation = customOperationFromPrism customOperationPrism1
        let currentCustomOperation = customOperationFromPrism customOperationPrism2

        let customOperationNamesEqual = offendingCustomOperation.Name = currentCustomOperation.Name
        let parametersEqual = parameters1 = parameters2

        customOperationNamesEqual && parametersEqual

let switchToTopTab (state : State) : State =
    let tab = state ^. State.CurrentTabPrism_ |> Option.get
    try
        let executionState =
            buildExecutionStateForCustomOperation state.CustomOperations tab.TabCustomOperationPrism tab.TabParameters
        state |> executionState ^= State.ExecutionState_
    with RecursionTrapException (offendingIconPrism, trapExecutionState) ->
        let trapInCurrentTab =
            executionContextEqual state (offendingIconPrism, trapExecutionState.Parameters) (tab.TabCustomOperationPrism, tab.TabParameters)
        if trapInCurrentTab then
            state |> trapExecutionState ^= State.ExecutionState_
        else
            onTrap offendingIconPrism trapExecutionState state

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
        | SetCustomOperationCreatorName name ->
            state |> name ^= (State.InputState_ >-> InputState.CustomOperationCreatorName_)
        | SetCustomOperationParameterCount count ->
            state |> count ^= (State.InputState_ >-> InputState.CustomOperationCreatorParameterCount_)
        | PressCustomOperationCreatorButton ->
            let customOperationName = state.InputState.CustomOperationCreatorName
            let parameterCountText = state.InputState.CustomOperationCreatorParameterCount
            match (isNumber parameterCountText, isCustomOperationNameValid customOperationName) with
            | (true, true) ->
                let parameterCount = int parameterCountText
                let customOperation = createCustomOperation customOperationName parameterCount
                state |> appendReverse [customOperation] ^% State.CustomOperations_
            | _ -> state
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
        let currentCustomOperationPrism =
            let optic = State.CurrentTabPrism_ >?> SingleTabState.TabCustomOperationPrism_
            state ^. optic |> Option.get // We expect only valid prisms to be used

        let choices = state.ExecutionState.CurrentBranchChoices
        let actionTreePrism =
            State.CustomOperations_ >-> currentCustomOperationPrism >?> CustomOperation.LocalActionTree_
        let stateWithAction =
            state |> appendNewActionToTree iconAction choices ^% actionTreePrism

        try
            stateWithAction
            |> applyExecutionActionNode stateWithAction.CustomOperations iconAction ^% State.ExecutionState_
            |> removeTopTabIfResultIsSome
        with RecursionTrapException (offendingCustomOperation, executionState) ->
            onTrap offendingCustomOperation executionState stateWithAction
    let createNewCustomOperation name parameterCount state =
        let nameAlreadyExists =
            state.CustomOperations |> List.exists (fun icon -> icon.Name = name)
        if nameAlreadyExists then
            state
        else
            let customOperation = createCustomOperation name parameterCount
            // We have to use append instead of cons to preserve the indices
            state |> listAppend customOperation ^% State.CustomOperations_

    match action with
    | CreateNewCustomOperation (name, parameterCount) ->
        createNewCustomOperation name parameterCount state
    | InputAction inputAction ->
        updateWithInputAction inputAction state
    | IconAction iconAction ->
        applyIconAction iconAction state
    | CloseInitialPopup ->
        state |> true ^= State.IntialPopupClosed_