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
        { Name = "!"; Op = fun x -> if x = FalseValue then TrueValue else FalseValue }
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

let private mouseEventPreventPropagation (e : Browser.Types.MouseEvent) =
    e.preventDefault()
    e.stopPropagation()

let wrapSimpleAction action =
    action |> wrapSimpleExecutionAction |> IconAction

let wrapBranchingAction action =
    action |> wrapBranchingExecutionAction |> IconAction

let private parameterToString (parameter : IconInstructionParameter) =
    match parameter with
    | Some number -> sprintf "%d" number
    | None -> ""

let stateIsHoldingObject state  =
    match state.ExecutionState.HeldObject with
    | NoObject -> false
    | _ -> true

let private renderIcon
    (state : State)
    (iconPrism : DrawnIconPrism)
    (icon : DrawnIcon)
    (dispatch : Action -> unit)
    : ReactElement =
    let dispatchSimple = wrapSimpleAction >> dispatch
    let stateIsHoldingObject = stateIsHoldingObject state

    let renderIconIOField =
        let renderParameter (index : int) (parameter : IconInstructionParameter) =
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

        let decorateIcon (icon : DrawnIcon) (renderedParameters : ReactElement list) =
            let iconDecoratorText (text : string) =
                Html.div [
                    prop.text text
                    prop.className "icon-decorator"
                ]

            match icon.IconInstruction with
            | Unary (op, _) ->
                [ iconDecoratorText op.Name ; renderedParameters[0] ]
            | Binary (op, _, _) ->
                [ renderedParameters[0]; iconDecoratorText op.Name; renderedParameters[1] ]
            | If (_) -> [
                    iconDecoratorText "If"
                    renderedParameters[0]
                ]
            | CallCustomIcon (prism, _) ->
                let name =
                    state ^. (State.CustomIcons_ >-> prism >?> CustomIcon.Name_)
                    |> Option.defaultValue "Invalid Icon Call"
                iconDecoratorText name :: renderedParameters

        let IOField =
            match icon.Result with
            | Some result ->
                Html.div [
                    prop.className "icon-result"
                    prop.text (sprintf "%d" result)
                ]
            | _ ->
                Html.div [
                    prop.className "icon-io-field"
                    prop.children (
                        List.mapi renderParameter (icon ^. (DrawnIcon.IconInstruction_ >-> IconInstruction.Params_))
                        |> decorateIcon icon )
                ]
        IOField

    let renderIconActions =
        let removeHandler e =
            mouseEventPreventPropagation e
            RemoveIcon id |> dispatchSimple
        let getResult e =
            mouseEventPreventPropagation e
            iconPrism
            |> PickupIconResult
            |> dispatchSimple
        let evalHandler e =
            mouseEventPreventPropagation e
            match icon.IconInstruction with
            | Unary _ -> EvaluateSimpleIcon iconPrism |> dispatchSimple
            | Binary _ -> EvaluateSimpleIcon iconPrism |> dispatchSimple
            | If _ -> EvaluateBranchingIcon iconPrism |> wrapBranchingAction |> dispatch
            | CallCustomIcon _ -> EvaluateSimpleIcon iconPrism |> dispatchSimple
        let moveHandler e =
            mouseEventPreventPropagation e
            PickupIcon iconPrism |> dispatchSimple

        let iconIsEvaluated = icon.Result.IsSome

        let createButton (name : string) action (className : string) =
            Html.button [
                prop.text name
                prop.onClick action
                prop.disabled stateIsHoldingObject
                prop.className className
            ]
        let removeButton =
            createButton "Remove" removeHandler "icon-remove-button"
        let getResultButton =
            createButton "Get Result" getResult "icon-get-result-button"
        let evalButton =
            createButton "Eval" evalHandler "icon-eval-button"
        let moveButton =
            createButton "Move" moveHandler "icon-move-button"
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

    let icon =
        Html.div [
            prop.className "icon"
            prop.style [
                style.left (length.px icon.X)
                style.top (length.px icon.Y)
            ]
            prop.children [
                renderIconIOField
                renderIconActions
            ]
        ]
    icon

let private renderIconInstances (state : State) (dispatch : Action -> unit) : ReactElement list =
    state.ExecutionState.LocalIcons
    |> List.mapi (fun index icon -> renderIcon state (List.pos_ index) icon dispatch)

let private defaultIconSpawnersView (dispatch : Action -> unit) : ReactElement =
    let spawnerView (text : string) (iconInstruction : IconInstruction) (dispatch : Action -> unit) : ReactElement =
        Html.button [
            prop.className "spawner-button"
            prop.text text
            prop.onClick (fun _ -> PickupNewIcon iconInstruction |> wrapSimpleAction |> dispatch)
        ]
    let spawnerListTemplate (sectionName : string) iconSource =
        let spawnerList =
            List.map
                (fun (text, instruction) ->
                    Html.li [ spawnerView text instruction dispatch ])
        Html.div [
            Html.h4 sectionName
            Html.ul [
                prop.children (spawnerList iconSource)
            ]
        ]
    let defaultIconSpawners =
        let unaryOperatorsSpawnerList =
            List.map (fun (op : UnaryOperation) ->
                (op.Name, Unary (op, None))) defaultUnaryOperators
        let binaryOperatorsSpawnerList =
            List.map (fun (op : BinaryOperation) ->
                (op.Name, Binary (op, None, None))) defaultBinaryOperators
        Html.div [
            prop.id "default-icon-spawners"
            prop.children [
                spawnerListTemplate
                    "Unary"
                    unaryOperatorsSpawnerList
                spawnerListTemplate
                    "Binary"
                    binaryOperatorsSpawnerList
                spawnerListTemplate
                    "Special"
                    [ ("If", If None)]
            ]
        ]
    defaultIconSpawners

let private customIconSpawnersView (state : State) (dispatch : Action -> unit) : ReactElement =
    let customIconSpawnerView (customIcon : CustomIcon) (iconPrism : CustomIconPrism) =
        let iconName = customIcon.Name
        Html.div [
            prop.id "custom-icon-spawners"
            prop.children [
                Html.button [
                    prop.text iconName
                    if not (customIconNameContainsInvalidCharacter iconName) then
                        prop.onClick (fun _ ->
                            (iconPrism, List.init customIcon.ParameterCount (fun _ -> None))
                            |> CallCustomIcon
                            |> PickupNewIcon
                            |> wrapSimpleAction
                            |> dispatch )
                ]
            ]
        ]
    let customIconsSpawners =
        Html.div (
            state.CustomIcons
            |> List.filter (fun icon -> icon.Name <> initialCustomIconName)
            |> List.mapi
                (fun index icon ->
                    customIconSpawnerView icon (List.pos_ index) ))
    customIconsSpawners
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

let private heldObjectView (state : State) =
    let heldObjectToString =
        let object = state.ExecutionState.HeldObject
        match object with
        | NewIcon iconType -> sprintf "%A" iconType
        | Number number -> sprintf "%d" number
        | ExistingIcon _ -> "Moving existing icon"
        | _ -> String.Empty

    let heldObject =
        Html.div [
            prop.id "held-object"
            prop.text heldObjectToString
        ]

    heldObject

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
                |> List.map singleTabView
            )
        ]

    tabs

let private customIconCreatorView (state : State) (dispatch : Action -> unit) : ReactElement =
    let dispatchInput = InputAction >> dispatch
    let iconNameInput =
        Html.input [
            prop.className "custom-icon-creator-name-input"
            prop.type' "text"
            prop.placeholder "Icon Name"
            prop.onTextChange (fun newText ->
                SetCustomIconCreatorName newText |> dispatchInput )
        ]
    let iconParamCountInput =
        Html.input [
            prop.className "custom-icon-creator-param-count-input"
            prop.type' "number"
            prop.placeholder "Parameter Count"
            prop.onTextChange (fun newText ->
                SetCustomIconParameterCount newText |> dispatchInput )
        ]
    let createNewIconButton =
        let nameField = state.InputState.CustomIconCreatorName
        let paramCountField = state.InputState.CustomIconCreatorParameterCount

        let nameValid = isCustomIconNameValid nameField
        let paramCountValid = isNumber paramCountField

        let isInputValid = nameValid && paramCountValid

        Html.button [
            prop.disabled (not isInputValid)
            prop.text "New Icon"
            prop.onClick (fun _ ->
                (nameField, int paramCountField)
                |> CreateNewCustomIcon
                |> dispatch
            )
        ]
    let customIconCreator =
        Html.div [
            prop.id "custom-icon-creator"
            prop.children [
                iconNameInput
                iconParamCountInput
                createNewIconButton
            ]
        ]

    customIconCreator

let tabParametersView (state : State) (dispatch : Action -> unit) : ReactElement =
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

let renderIconCanvas (state : State) (dispatch : Action -> unit) : ReactElement =
    let canvasOnClick (e : Browser.Types.MouseEvent) =
        mouseEventPreventPropagation e
        (int e.clientX, int e.clientY)
        |> Position
        |> PlacePickup
        |> wrapSimpleAction
        |> dispatch


    let canvas =
        Html.div [
            prop.id "icon-canvas"
            prop.onClick canvasOnClick
            prop.children (renderIconInstances state dispatch)
        ]
    canvas

let render (state : State) (dispatch : Action -> unit) : ReactElement =
    Html.div [
        prop.id "root-container"
        prop.children [
            heldObjectView state
            tabParametersView state dispatch
            defaultIconSpawnersView dispatch
            customIconSpawnersView state dispatch
            renderIconCanvas state dispatch
            customIconCreatorView state dispatch
            constantSpawnerView state dispatch
            tabView state dispatch
        ]

        prop.onContextMenu (fun e ->
            if stateIsHoldingObject state then
                e.preventDefault()
                CancelPickup |> wrapSimpleAction |> dispatch )
    ]

