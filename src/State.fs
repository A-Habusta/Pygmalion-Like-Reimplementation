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

/// <summary>
/// Creates a custom operation with the given name and parameter count.
/// </summary>
/// <param name="name">The name of the custom operation.</param>
/// <param name="parameterCount">The number of parameters the custom operation takes.</param>
/// <returns>The created custom operation.</returns>
let private createCustomOperation name (parameterCount : int) : CustomOperation =
    { Name = name
      ActionTree = trap
      ParameterCount = parameterCount }

/// <summary>
/// Base state for the application.
/// </summary>
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


/// <summary>
/// Creates a tab with the given name and custom operation prism.
/// </summary>
/// <param name="tabName">The name of the tab.</param>
/// <param name="tabCustomOperationPrism">The prism for the custom operation of the tab.</param>
/// <param name="parameters">The parameters of the tab.</param>
/// <returns>The created tab.</returns>
let createTab tabName tabCustomOperationPrism parameters  =
    { TabName = tabName
      TabCustomOperationPrism = tabCustomOperationPrism
      TabParameters = parameters }

/// <summary>
/// Creates a tab from the given custom operation prism.
/// </summary>
/// <param name="customOperations">All custom operations of the state.</param>
/// <param name="customOperationPrism">The prism for the custom operation of the tab.</param>
/// <param name="parameters">The parameters of the tab.</param>
/// <returns>The created tab.</returns>
let createTabFromCustomOperation customOperations customOperationPrism parameters =
    let customOperation = customOperations ^. customOperationPrism |> Option.get
    let name = customOperation.Name
    createTab name customOperationPrism parameters

/// <summary>
/// Creates new tab for execution state received from a trap exception.
/// </summary>
/// <param name="prism">The prism for the custom operation which caused the trap.</param>
/// <param name="executionState">The execution state at the moment of hitting the trap.</param>
/// <param name="state">The current state of the application.</param>
/// <returns>The updated state with the new tab.</returns>
let onTrap prism executionState state =
    let tab = createTabFromCustomOperation state.CustomOperations prism executionState.Parameters
    state |> cons tab ^% State.Tabs_ |> executionState ^= State.ExecutionState_

/// <summary>
/// Checks if two custom operations are equal and have the same parameters.
/// </summary>
/// <param name="state">The current state of the application.</param>
/// <param name="customOperationPrism1">The prism for the first custom operation.</param>
/// <param name="parameters1">The parameters of the first custom operation.</param>
/// <param name="customOperationPrism2">The prism for the second custom operation.</param>
/// <param name="parameters2">The parameters of the second custom operation.</param>
/// <returns>True if the custom operations are equal and have the same parameters, false otherwise.</returns>
let executionContextEqual state (customOperationPrism1, parameters1) (customOperationPrism2, parameters2) =
        let customOperationFromPrism prism = state.CustomOperations ^. prism |> Option.get
        let offendingCustomOperation = customOperationFromPrism customOperationPrism1
        let currentCustomOperation = customOperationFromPrism customOperationPrism2

        let customOperationNamesEqual = offendingCustomOperation.Name = currentCustomOperation.Name
        let parametersEqual = parameters1 = parameters2

        customOperationNamesEqual && parametersEqual

/// <summary>
/// Tries to switch to the top tab of the state. Might have to handle a trap exception.
/// </summary>
/// <param name="state">The current state of the application.</param>
/// <returns> The updated state swtiched the top tab.</returns>
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

/// <summary>
/// Wraps an execution action within an action usable with the master state.
/// </summary>
/// <param name="action">The execution action to wrap.</param>
/// <returns>The wrapped action.</returns>
let wrapExecutionActionNode (action : ExecutionActionTree) : Action =
    IconAction action

/// <summary>
/// Updates the state based on the given action. Callled for every dispatched action.
/// </summary>
/// <param name="action">The action to update the state with.</param>
/// <param name="state">The current state of the application.</param>
/// <returns>The updated state.</returns>
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