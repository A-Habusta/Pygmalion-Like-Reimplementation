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
            icon.Result
            |> Option.defaultValue 0
            |> PickupNumber
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
    let customIconSpawnerView (iconTypeName : string) (iconPrism : CustomIconPrism) =
        Html.div [
            prop.id "custom-icon-spawners"
            prop.children [
                Html.button [
                    prop.text iconTypeName
                    if not (customIconNameContainsInvalidCharacter iconTypeName) then
                        prop.onClick (fun _ -> dispatch (PickupNewIcon (CustomIcon (iconTypeName, iconType.ParameterCount))))
                ]
                Html.button [
                    prop.text "Edit"
                    prop.onClick (fun _ -> dispatch (EditCustomIcon iconTypeName))
                ]
            ]
        ]
    let customIconsSpawners =
        Html.div ( state.CustomIcons
            |> List.filter (fun (name, _) -> name <> dummyCustomIconName)
            |> List.map
                (fun (id, icon) ->
                    let name = CustomIconNameRemoveInvisiblePart id
                    customIconSpawnerView name icon) )
    customIconsSpawners
let private constantSpawnerView (state : State) (dispatch : Message -> unit) : ReactElement =
    Html.div [
        prop.id "constant-spawner"
        prop.children [
            Html.input [
                prop.className "constant-spawner-text"
                prop.type' "number"
                prop.placeholder "Constant"
                prop.onTextChange (fun newText ->
                    dispatch (ChangeConstantSpawnerText newText) )
            ]
            Html.button [
                prop.className "constant-spawner-button"
                prop.text "Pickup"
                prop.onClick (fun _ ->
                    match state.ConstantSpawnerText with
                    | text when isNumber text ->
                        dispatch (PickupIconParameter (Constant (int text)))
                    | _ -> () )
                isNumber state.ConstantSpawnerText
                |> not
                |> prop.disabled
            ]
        ]
    ]

let private heldObjectView (state : State) =
    let heldObjectToString =
        let object = state.HeldObject
        match object with
        | NewIcon iconType -> sprintf "%A" iconType
        | Parameter parameter ->
            parameterToString state parameter
        | ExistingIcon _ -> "Moving existing icon"
        | _ -> String.Empty

    let heldObject =
        Html.div [
            prop.id "held-object"
            prop.text heldObjectToString
        ]

    heldObject

let private tabView (state : State) (dispatch : Message -> unit) : ReactElement =
    let singleTabView (tabIndex : int) (tab : TabState) =
        let isCurrentTab = tabIndex = state.CurrentTabIndex
        let isMainTab = tabIndex = 0
        let button =
            Html.button [
                prop.disabled isCurrentTab
                prop.text tab.Name
                prop.onClick (fun _ -> dispatch (SwitchToTab tabIndex))
            ]
        let removeButton =
            Html.button [
                prop.text "X"
                prop.onClick (fun _ -> dispatch (RemoveTab tabIndex))

            ]
        let tab =
            Html.div [
                prop.className "tab"
                prop.children [
                    button
                    if not (isCurrentTab || isMainTab) then removeButton
                ]
            ]
        tab

    let tabs =
        Html.div [
            prop.id "tabs"
            prop.children (
                state.Tabs
                |> List.mapi singleTabView
            )
        ]

    tabs

let private customIconCreatorView (state : State) (dispatch : Message -> unit) : ReactElement =
    let iconNameInput =
        Html.input [
            prop.className "custom-icon-creator-name-input"
            prop.type' "text"
            prop.placeholder "Icon Name"
            prop.onTextChange (fun newText ->
                dispatch (ChangeCustomIconCreatorName newText) )
        ]
    let iconParamCountInput =
        Html.input [
            prop.className "custom-icon-creator-param-count-input"
            prop.type' "number"
            prop.placeholder "Parameter Count"
            prop.onTextChange (fun newText ->
                dispatch (ChangeCustomIconCreatorParameterCount newText) )
        ]
    let createNewIconButton =
        let isInputValid = isText state.CustomIconCreatorName && isNumber state.CustomIconCreatorParameterCount
        Html.button [
            prop.disabled (not isInputValid)
            prop.text "New Icon"
            prop.onClick (fun _ ->
                dispatch (CreateCustomIcon(state.CustomIconCreatorName, int state.CustomIconCreatorParameterCount))
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

let tabParametersView (state : State) (dispatch : Message -> unit) : ReactElement =
    let drawParameter index (parameter : int) =
        let parameterText =
            sprintf "%d" parameter
        Html.div [
            prop.className "tab-parameter"
            prop.text parameterText
            prop.onClick (fun _ -> dispatch (PickupIconParameter (BaseIconParameter index)))
        ]
    let parameters =
        Html.div [
            prop.id "tab-parameters"
            prop.children
                ( getCurrentTab state
                |> fun tab -> tab.MasterCustomIconParameters
                |> List.mapi drawParameter )
        ]
    parameters

let renderIconCanvas (state : State) (dispatch : Message -> unit) : ReactElement =
    let canvasOnClick (e : Browser.Types.MouseEvent) =
        mouseEventPreventPropagation e
        dispatch (PlacePickup (Position (int e.clientX, int e.clientY)))
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
                dispatch CancelPickup )
    ]

