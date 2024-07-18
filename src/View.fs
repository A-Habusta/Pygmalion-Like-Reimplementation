module PygmalionReimplementation.View

open System
open Feliz

open Aether
open Aether.Operators

open PygmalionReimplementation.SimpleEval
open PygmalionReimplementation.State
open PygmalionReimplementation.Icons
open PygmalionReimplementation.Utils

let private defaultUnaryOperators : UnaryOperation list =
    [
        { Name = "+"; Op = id }
        { Name = "-"; Op = fun x -> -x }
        { Name = "not"; Op = fun x -> if x = FalseValue then TrueValue else FalseValue }
    ]

let private boolOutputToInt = binaryFuncResultConverter boolToInt
let private defaultBinaryOperators =
    [
        { Name = "+"; Op = (+) }
        { Name = "-"; Op = (-) }
        { Name = "*"; Op = (*) }
        { Name = "/"; Op = (/) }
        { Name = "%"; Op = (%) }

        { Name = "="; Op = boolOutputToInt (=) }
        { Name = "<>"; Op = boolOutputToInt (<>) }
        { Name = "<"; Op = boolOutputToInt (<) }
        { Name = "<="; Op = boolOutputToInt (<=) }
        { Name = ">"; Op = boolOutputToInt (>) }
        { Name = ">="; Op = boolOutputToInt (>=) }

        { Name = "&&"; Op = boolOutputToInt (fun x y -> x = FalseValue || y = FalseValue |> not) }
        { Name = "||"; Op = boolOutputToInt (fun x y -> x = FalseValue && y = FalseValue |> not) }
    ]

/// <summary>
/// Prevents the default action of the event and stops it from propagating.
/// Primarily used to prevent clicks registering on multiple targets.
/// </summary>
let private mouseEventPreventPropagation (e : Browser.Types.MouseEvent) =
    e.preventDefault()
    e.stopPropagation()

let wrapSimpleAction action =
    action |> wrapSimpleExecutionAction |> IconAction

let wrapBranchingAction action =
    action |> wrapBranchingExecutionAction |> IconAction

let private parameterToString (parameter : IconOperationParameter) =
    match parameter with
    | Some number -> sprintf "%d" number
    | None -> ""

let stateIsHoldingObject state  =
    match state.ExecutionState.HeldObject with
    | NoObject -> false
    | _ -> true

/// <summary>
/// Renders a single icon.
/// </summary>
/// <param name="state">The current state of the program.</param>
/// <param name="dispatch">The dispatch function to send messages to the update function.</param>
/// <param name="iconIndex">The index of the icon in the list of icons.</param>
/// <param name="icon">The icon to render.</param>
let private renderIcon
    (state : State)
    (dispatch : Action -> unit)
    (iconIndex : int)
    (icon : Icon)
    : ReactElement =
    let dispatchSimple = wrapSimpleAction >> dispatch
    let stateIsHoldingObject = stateIsHoldingObject state
    let iconPrism = List.pos_ iconIndex

    // Renders the icon's input/output field, which displays the icons parameters or result.
    let renderIconIOField =
        let renderParameter (index : int) (parameter : IconOperationParameter) =
            let deleteButton =
                Html.div [
                    prop.text "X"
                    prop.className "icon-parameter-delete"
                    prop.onClick (fun e ->
                        mouseEventPreventPropagation e
                        RemoveIconParameter(iconPrism, index) |> dispatchSimple )
                ]
            let text =
                Html.div [
                    prop.text (parameterToString parameter)
                    prop.className "icon-parameter-text"
                ]
            let parameterElement =
                Html.div [
                    prop.onClick (fun e ->
                        mouseEventPreventPropagation e
                        IconParameter(iconPrism, index) |> PlacePickup |> dispatchSimple )
                    prop.children [
                        text
                        if parameter.IsSome then
                            deleteButton
                    ]
                    prop.className "icon-parameter"
                ]
            parameterElement

        let getIconName =
            function
            | Unary (op, _) -> op.Name
            | Binary (op, _, _) -> op.Name
            | If _ -> "If"
            | CallCustomOperation (prism, _) ->
                state ^. (State.CustomOperations_ >-> prism >?> CustomOperation.Name_)
                |> Option.defaultValue "Invalid Icon Call"
        let iconDecoratorText (text : string) =
            Html.div [
                prop.text text
                prop.className "icon-decorator"
            ]
        let decorateIcon (icon : Icon) (renderedParameters : ReactElement list) =
            match icon.Operation with
            | Unary (op, _) ->
                [ iconDecoratorText op.Name ; renderedParameters[0] ]
            | Binary (op, _, _) ->
                [ renderedParameters[0]; iconDecoratorText op.Name; renderedParameters[1] ]
            | If (_) ->
                    [  iconDecoratorText "If"
                       renderedParameters[0] ]
            | CallCustomOperation _ ->
                let name = getIconName icon.Operation
                iconDecoratorText name :: renderedParameters

        let IOField =
            match icon.Result with
            | Some result ->
                Html.div [
                    prop.className "icon-result"
                    prop.children [
                        iconDecoratorText (sprintf "(%s)" (getIconName icon.Operation))
                        iconDecoratorText (sprintf "%d" result)
                    ]
                ]
            | _ ->
                Html.div [
                    prop.className "icon-io-field"
                    prop.children (
                        List.mapi renderParameter (icon ^. (Icon.Operation_ >-> IconOperation.Params_))
                        |> decorateIcon icon )
                ]
        IOField

    // Render the buttons that allow the user to interact with the icon.
    let renderIconActions =
        let removeHandler e =
            mouseEventPreventPropagation e
            listRemoveIndex iconIndex |> RemoveIcon |> dispatchSimple
        let getResult e =
            mouseEventPreventPropagation e
            iconPrism
            |> PickupIconResult
            |> dispatchSimple
        let evalHandler e =
            mouseEventPreventPropagation e
            match icon.Operation with
            | Unary _ -> EvaluateSimpleIcon iconPrism |> dispatchSimple
            | Binary _ -> EvaluateSimpleIcon iconPrism |> dispatchSimple
            | If _ -> EvaluateBranchingIcon iconPrism |> wrapBranchingAction |> dispatch
            | CallCustomOperation _ -> EvaluateSimpleIcon iconPrism |> dispatchSimple
        let moveHandler e =
            mouseEventPreventPropagation e
            PickupIcon iconPrism |> dispatchSimple

        let iconIsEvaluated = icon.Result.IsSome

        let createButton action (className : string ) (iconName : string)=
            Html.button [
                prop.onClick action
                prop.disabled stateIsHoldingObject
                prop.className className
                prop.children [
                    Html.i [
                        prop.classes ["fa"; iconName]
                    ]
                ]
            ]
        let removeButton =
            createButton removeHandler "icon-remove-button" "fa-trash"
        let getResultButton =
            createButton getResult "icon-get-result-button" "fa-hand-grab-o"
        let evalButton =
            createButton evalHandler "icon-eval-button" "fa-play"
        let moveButton =
            createButton moveHandler "icon-move-button" "fa-arrows"
        let buttons =
            Html.div [
                prop.className "icon-button-container"
                prop.children [
                    removeButton
                    if iconIsEvaluated then getResultButton else evalButton
                    moveButton
                ]
            ]

        buttons

    // Build the icon element.
    let icon =
        Html.div [
            prop.className "icon"
            prop.style [
                style.left (length.px icon.X)
                style.top (length.px icon.Y)
                style.zIndex icon.Z
            ]
            prop.children [
                renderIconIOField
                renderIconActions
            ]
            prop.onClick mouseEventPreventPropagation
        ]
    icon

/// <summary>
/// Renders all icons in the current state.
/// </summary>
/// <param name="state">The current state of the program.</param>
/// <param name="dispatch">The dispatch function to send messages to the update function.</param>
/// <returns>A list of ReactElements, each representing a single icon.</returns>
let private renderIcons (state : State) (dispatch : Action -> unit) : ReactElement list =
    state.ExecutionState.LocalIcons
    |> List.mapi (renderIcon state dispatch)

/// <summary>
/// Renders a single icon spawner, which is the button used for creating a new icon.
/// </summary>
/// <param name="text">The text to display on the button.</param>
/// <param name="iconOperation">The operation that the icon created with this button should have.</param>
/// <param name="dispatch">The dispatch function to send messages to the update function.</param>
/// <returns>A ReactElement representing the icon spawner.</returns>
let private iconSpawnerView (text : string) (iconOperation : IconOperation) (dispatch : Action -> unit) : ReactElement =
    Html.button [
        prop.className "icon-spawner-button"
        prop.text text
        prop.onClick (fun _ -> PickupNewIcon iconOperation |> wrapSimpleAction |> dispatch)
    ]
/// <summary>
/// Renders a list of icon spawners.
/// </summary>
/// <param name="dispatch">The dispatch function to send messages to the update function.</param>
/// <param name="sectionName">The name of the section that the icon spawners belong to.</param>
/// <param name="iconSource">A list of tuples, where the first element is the text to display on the button,
/// and the second element is the operation that the icon created with this button should have.</param>
let private iconSpawnerListTemplate dispatch (sectionName : string) iconSource =
    let iconSpawnerList =
        List.map
            (fun (text, operation) ->
                Html.li [ iconSpawnerView text operation dispatch ])
    Html.div [
        Html.h4 sectionName
        Html.ul [
            prop.children (iconSpawnerList iconSource)
        ]
    ]
/// <summary>
/// Renders spawners for pre-defined icons.
/// </summary>
/// <param name="dispatch">The dispatch function to send messages to the update function.</param>
/// <returns>A ReactElement representing the default icon spawners.</returns>
let private defaultIconSpawnersView (dispatch : Action -> unit) : ReactElement =
    let boundIconSpawnerListTemplate = iconSpawnerListTemplate dispatch
    let defaultIconSpawners =
        let unaryOperatorsSpawnerBaseList =
            List.map (fun (op : UnaryOperation) ->
                (op.Name, Unary (op, None))) defaultUnaryOperators
        let binaryOperatorsSpawnerBaseList =
            List.map (fun (op : BinaryOperation) ->
                (op.Name, Binary (op, None, None))) defaultBinaryOperators
        Html.div [
            prop.id "default-icon-spawners"
            prop.children [
                boundIconSpawnerListTemplate
                    "Unary"
                    unaryOperatorsSpawnerBaseList
                boundIconSpawnerListTemplate
                    "Binary"
                    binaryOperatorsSpawnerBaseList
                boundIconSpawnerListTemplate
                    "Special"
                    [ ("If", If None)]
            ]
        ]
    defaultIconSpawners

/// <summary>
/// Renders spawners for custom icons.
/// </summary>
/// <param name="state">The current state of the program.</param>
/// <param name="dispatch">The dispatch function to send messages to the update function.</param>
/// <returns>A ReactElement representing the custom icon spawners.</returns>
let private customOperationSpawnersView (state : State) (dispatch : Action -> unit) : ReactElement =
    let customOperationSpawnerBaseList =
        state.CustomOperations
        |> List.mapi (fun index icon -> (index, icon)) // Done like this to preserve the index
        |> List.filter (fun (_, icon) -> icon.Name <> initialCustomOperationName)
        |> List.map (fun (index, icon) ->
            let iconParameters = List.init icon.ParameterCount (fun _ -> None)
            let iconPrism = List.pos_ index
            let finalIconOperation = CallCustomOperation (iconPrism, iconParameters)
            (icon.Name, finalIconOperation) )
    Html.div [
        prop.id "custom-icon-spawners"
        prop.children [
            iconSpawnerListTemplate dispatch "Custom" customOperationSpawnerBaseList
        ]
    ]
/// <summary>
/// Renders the constant spawner, which is an input used for creating constant values.
/// </summary>
/// <param name="state">The current state of the program.</param>
/// <param name="dispatch">The dispatch function to send messages to the update function.</param>
/// <returns>A ReactElement representing the constant spawner.</returns>
let private constantSpawnerView (state : State) (dispatch : Action -> unit) : ReactElement =
    let dispatchInput = InputAction >> dispatch
    Html.div [
        prop.id "constant-spawner"
        prop.children [
            Html.input [
                prop.className "constant-spawner-text"
                prop.type' "number"
                prop.placeholder "Constant"
                prop.onTextChange (fun newText ->
                    SetConstantSpawnerText newText |> dispatchInput )
            ]
            Html.button [
                prop.className "constant-spawner-button"
                prop.text "Pickup"
                prop.onClick (fun _ -> PressConstantSpawnerButton |> dispatchInput)
                prop.disabled (isNumber state.InputState.ConstantSpawnerText |> not)
            ]
        ]
    ]

/// <summary>
/// Renders the held object view, which displays the currently held object.
/// </summary>
/// <param name="state">The current state of the program.</param>
/// <returns>A ReactElement representing the held object view.</returns>
let private heldObjectView (state : State) =
    let renderIconOperation operation =
        match operation with
        | Unary (op, _) -> sprintf "Unary %s" op.Name
        | Binary (op, _, _) -> sprintf "Binary %s" op.Name
        | If _ -> "If"
        | CallCustomOperation _ -> "Call Custom Operation"
    let heldObjectToString =
        let object = state.ExecutionState.HeldObject
        match object with
        | NewIcon iconOperation -> renderIconOperation iconOperation
        | Number number -> sprintf "%d" number
        | ExistingIcon _ -> "Moving existing icon"
        | _ -> String.Empty

    let heldObject =
        Html.div [
            prop.id "held-object-container"
            prop.children [
                Html.p [
                    prop.id "held-object-text"
                    prop.text heldObjectToString
                ]
            ]
        ]

    heldObject

/// <summary>
/// Renders the tab view, which displays the currently open tabs.
/// </summary>
/// <param name="state">The current state of the program.</param>
/// <param name="dispatch">The dispatch function to send messages to the update function.</param>
/// <returns>A ReactElement representing the tab view.</returns>
let private tabView (state : State) (dispatch : Action -> unit) : ReactElement =
    let singleTabView (tab : SingleTabState) =
        Html.div [
            prop.className "tab"
            prop.text tab.TabName
        ]

    let tabs =
        Html.div [
            prop.id "tabs"
            prop.children (
                state.Tabs
                |> List.rev
                |> List.map singleTabView
            )
        ]

    tabs

/// <summary>
/// Renders the custom operation creator view, which allows the user to create new custom operations.
/// </summary>
/// <param name="state">The current state of the program.</param>
/// <param name="dispatch">The dispatch function to send messages to the update function.</param>
/// <returns>A ReactElement representing the custom operation creator view.</returns>
let private customOperationCreatorView (state : State) (dispatch : Action -> unit) : ReactElement =
    let dispatchInput = InputAction >> dispatch
    let iconNameInput =
        Html.input [
            prop.className "custom-icon-creator-name-input"
            prop.type' "text"
            prop.placeholder "Operation Name"
            prop.onTextChange (fun newText ->
                SetCustomOperationCreatorName newText |> dispatchInput )
        ]
    let iconParamCountInput =
        Html.input [
            prop.className "custom-icon-creator-param-count-input"
            prop.type' "number"
            prop.placeholder "Parameter Count"
            prop.onTextChange (fun newText ->
                SetCustomOperationParameterCount newText |> dispatchInput )
        ]
    let createNewIconButton =
        let nameField = state.InputState.CustomOperationCreatorName
        let paramCountField = state.InputState.CustomOperationCreatorParameterCount

        let nameValid = isCustomOperationNameValid nameField
        let paramCountValid = isNumber paramCountField

        let isInputValid = nameValid && paramCountValid

        Html.button [
            prop.className "custom-icon-creator-create-button"
            prop.disabled (not isInputValid)
            prop.text "New Operation"
            prop.onClick (fun _ ->
                (nameField, int paramCountField)
                |> CreateNewCustomOperation
                |> dispatch
            )
        ]
    let customOperationCreator =
        Html.div [
            prop.id "custom-icon-creator"
            prop.children [
                iconNameInput
                iconParamCountInput
                createNewIconButton
            ]
        ]

    customOperationCreator

/// <summary>
/// Renders the tab parameters view, which displays the parameters of the currently open tab.
/// </summary>
/// <param name="state">The current state of the program.</param>
/// <param name="dispatch">The dispatch function to send messages to the update function.</param>
/// <returns>A ReactElement representing the tab parameters view.</returns>
let private tabParametersView (state : State) (dispatch : Action -> unit) : ReactElement =
    let drawParameter index (parameter : int) =
        let parameterText =
            sprintf "%d" parameter
        Html.div [
            prop.className "tab-parameter"
            prop.text parameterText
            prop.onClick (fun _ ->
                PickupExecutionStateParameter index
                |> wrapSimpleAction
                |> dispatch )
        ]
    let parameters =
        Html.div [
            prop.id "tab-parameters"
            prop.children (
                state ^. (State.Tabs_ >-> List.head_ >?> SingleTabState.TabParameters_)
                |> Option.get
                |> List.mapi drawParameter )
        ]
    parameters
/// <summary>
/// Renders the result field view, which displays a box for storing the result of the current tab.
/// </summary>
/// <param name="state">The current state of the program.</param>
/// <param name="dispatch">The dispatch function to send messages to the update function.</param>
/// <returns>A ReactElement representing the result field view.</returns>
let private resultFieldView (state : State) (dispatch : Action -> unit) : ReactElement =
    let saveResult e =
        mouseEventPreventPropagation e
        SaveResult |> End |> IconAction |> dispatch
    let resultFieldChildren =
        prop.children [
            Html.text "Result: "
            Html.div [
                prop.id "result-field-box"
                prop.onClick (
                    match state.ExecutionState.HeldObject with
                    | Number _ -> saveResult
                    | _ -> mouseEventPreventPropagation )
            ]
        ]

    let resultField =
        Html.div [
            prop.id "result-field"
            if state.Tabs.Length > 1 then resultFieldChildren
        ]

    resultField

/// <summary>
/// Renders the icon canvas, which is the area where the user can place icons.
/// </summary>
/// <param name="state">The current state of the program.</param>
/// <param name="dispatch">The dispatch function to send messages to the update function.</param>
/// <returns>A ReactElement representing the icon canvas.</returns>
let private iconCanvas (state : State) (dispatch : Action -> unit) : ReactElement =
    let canvasOnClick (e : Browser.Types.MouseEvent) =
        mouseEventPreventPropagation e
        let target = e.target :?> Browser.Types.Element
        let canvasRect = target.getBoundingClientRect()
        (int (e.clientX - canvasRect.left), int (e.clientY - canvasRect.top))
        |> Position
        |> PlacePickup
        |> wrapSimpleAction
        |> dispatch


    let canvas =
        Html.div [
            prop.id "icon-canvas"
            prop.onClick canvasOnClick
            prop.children (renderIcons state dispatch)
        ]
    canvas

/// <summary>
/// Renders the entire application.
/// </summary>
/// <param name="state">The current state of the program.</param>
/// <param name="dispatch">The dispatch function to send messages to the update function.</param>
/// <returns>A ReactElement representing the entire application.</returns>
let private renderProgram (state : State) (dispatch : Action -> unit) : ReactElement =
    Html.div [
        prop.id "root-container"
        prop.children [
            heldObjectView state
            tabParametersView state dispatch
            defaultIconSpawnersView dispatch
            customOperationSpawnersView state dispatch
            iconCanvas state dispatch
            customOperationCreatorView state dispatch
            constantSpawnerView state dispatch
            resultFieldView state dispatch
            tabView state dispatch
        ]

        prop.onContextMenu (fun e ->
            if stateIsHoldingObject state then
                e.preventDefault()
                CancelPickup |> wrapSimpleAction |> dispatch )
    ]

/// <summary>
/// Renders the initial popup that is displayed when the application is first opened.
/// </summary>
/// <param name="dispatch">The dispatch function to send messages to the update function.</param>
/// <returns>A ReactElement representing the initial popup.</returns>
let private renderIntialPopup dispatch =
    let popupText =
        [ Html.h3 "Before you begin!"
          Html.p "This web application is the output program of my bachelors thesis, written at the Charles University Faculty of Mathematics and Science."
          Html.p "The goal of this thesis was not to create a fully fledged programming system. Rather, it was to explore how the original system might have worked."
          Html.p "For a quick guide on how to use the system, or  more in-depth information about the topic, you can read my thesis here:"
          Html.a [
              prop.href "https://a-habusta.github.io/bachelors-thesis/thesis.pdf"
              prop.text "Thesis"
          ]
          Html.p "Original programming system specification this entire project is based on:"
          Html.a [
              prop.href "https://apps.dtic.mil/sti/tr/pdf/ADA016811.pdf"
              prop.text "PDF Link"
          ]
          Html.p "Click anywhere to dismiss this popup." ]

    Html.div [
        prop.id "initial-popup"
        prop.children [
            Html.div [
                prop.id "initial-popup-content"
                prop.children popupText
            ]
        ]
    ]


/// <summary>
/// Renders the entire application, with an overlayed popup at the start.
///  The popup is displayed until the user clicks on it.
/// </summary>
/// <param name="state">The current state of the program.</param>
/// <param name="dispatch">The dispatch function to send messages to the update function.</param>
/// <returns>A ReactElement representing the entire application.</returns>
/// <remarks>This function is called by the Elmish program.</remarks>
let render (state : State) (dispatch : Action -> unit) =
    if state.IntialPopupClosed then
        // Render regular page
        renderProgram state dispatch
    else
        // Render page with overlayed popup, ignoring all messages from regular page
        Html.div [
            prop.children [
                renderIntialPopup dispatch
                // Pass ignore as dispatch so no messages are actually dispatched
                renderProgram state ignore
            ]
            prop.onClick (fun _ -> CloseInitialPopup |> dispatch)
        ]

