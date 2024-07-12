module PygmalionReimplementation.View

open System
open Feliz

open PygmalionReimplementation.State
open PygmalionReimplementation.Icons
open PygmalionReimplementation.Utils

let private defaultUnaryOperators =
    [ "+"; "-"; "!" ]
let private defaultBinaryOperators =
    [ "+"; "-"; "*"; "/"; "%"; "="; "<>"; "<"; "<="; ">"; ">="; "&&"; "||"; ]

let private unknownIdentifier = "?"

let private stateIsHoldingObject (state : State) =
    match state.HeldObject with
    | NoObject -> false
    | _ -> true

let private iconIsHeld (state : State) (id : IconID) =
    match state.HeldObject with
    | ExistingIcon heldId -> heldId = id
    | _ -> false

let private mouseEventPreventPropagation (e : Browser.Types.MouseEvent) =
    e.preventDefault()
    e.stopPropagation()

let private parameterToString (state : State) (parameter : IconOperationParameter) =
    match parameter with
    | LocalIconReference id ->
        match getIconResultFromState state id with
        | Some result -> sprintf "%A" result
        | None -> unknownIdentifier
    | OperationParameter position ->
        let param = getMasterCustomOperationParameters state |> List.item position
        sprintf "%d" param
    | Constant value -> sprintf "%d" value
    | Trap -> String.Empty

let private renderIcon
    (icon : Icon)
    (id : IconID)
    (state : State)
    (dispatch : Message -> unit) : ReactElement =
    let renderIconIOField =
        let renderParameter (index : int) (parameter : IconOperationParameter) =
            let deleteButton =
                Html.div [
                    prop.text "X"
                    prop.onClick (fun e ->
                        mouseEventPreventPropagation e
                        dispatch (RemoveIconParameter(id, index)) )
                    prop.className "icon-parameter-delete"
                ]
            let text =
                Html.div [
                    prop.text (parameterToString state parameter)
                    prop.className "icon-parameter-text"
                ]
            let parameter =
                Html.div [
                    prop.onClick (fun e ->
                        mouseEventPreventPropagation e
                        dispatch (PlacePickup(IconParameter(id, index))) )
                    prop.children [
                        text
                        if parameter <> Trap then
                            deleteButton
                    ]
                    prop.className "icon-parameter"
                ]
            parameter

        let decorateIcon (icon : Icon) (renderedParameters : ReactElement list) =
            let iconDecoratorText (text : string) =
                Html.div [
                    prop.text text
                    prop.className "icon-decorator"
                ]

            match icon.Operation with
            | Unary (op, _) ->
                [ iconDecoratorText op; renderedParameters[0] ]
            | Binary (op, _, _) ->
                [ renderedParameters[0]; iconDecoratorText op; renderedParameters[1] ]
            | If (_) -> [
                    let visibleId = GetIconIdCharacters id IconVisibleIdCharacters
                    let ifName = $"If {visibleId}"
                    iconDecoratorText ifName
                    renderedParameters[0]
                ]
            | CallCustomOperation (name, _) ->
                iconDecoratorText name :: renderedParameters
            | TopLevelTrap -> [ Html.text "Trap" ]

        let IOField =
            match getIconResultFromState state id with
            | Some result ->
                Html.div [
                    prop.className "icon-result"
                    prop.text (sprintf "%d" result)
                ]
            | None ->
                Html.div [
                    prop.className "icon-io-field"
                    prop.children (
                        List.mapi renderParameter (extractOperationParameters icon.Operation)
                        |> decorateIcon icon )
                ]
        IOField

    let renderIconActions =
        let removeHandler e =
            mouseEventPreventPropagation e
            dispatch (RemoveIcon id)
        let getReferenceHandler e =
            dispatch (PickupIconParameter (LocalIconReference(id)))
        let evalHandler e =
            mouseEventPreventPropagation e
            dispatch (EvaluateIcon(id))
        let moveHandler e =
            mouseEventPreventPropagation e
            dispatch (PickupIcon id)

        let iconIsEvaluated = Option.isSome (getIconResultFromState state id)

        let removeButton =
            Html.button [
                prop.text "Remove"
                prop.onClick removeHandler
                prop.disabled (iconIsHeld state id)
                prop.className "icon-remove-button"
            ]
        let getReferenceButton =
            Html.button [
                prop.text "Get Reference"
                prop.onClick getReferenceHandler
                prop.disabled (iconIsHeld state id)
                prop.className "icon-get-reference-button"
            ]
        let evalButton =
            Html.button [
                prop.text "Eval"
                prop.onClick evalHandler
                prop.disabled (iconIsHeld state id || iconIsEvaluated)
                prop.className "icon-eval-button"
            ]
        let moveButton =
            Html.button [
                prop.text "Move"
                prop.onClick moveHandler
                prop.disabled (iconIsHeld state id)
                prop.className "icon-move-button"
            ]
        let buttons =
            Html.div [
                prop.className "icon-button-container"
                prop.children [
                    removeButton
                    getReferenceButton
                    evalButton
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

let private renderIcons (state : State) (dispatch : Message -> unit) : ReactElement list =
    getIconTableFromState state
    |> Map.toList
    |> List.map (fun (id, icon) -> renderIcon icon id state dispatch)

let private defaultIconSpawnersView (dispatch : Message -> unit) : ReactElement =
    let spawnerView (text : string) (iconType : IconType) (dispatch : Message -> unit) : ReactElement =
        Html.button [
            prop.className "spawner-button"
            prop.text text
            prop.onClick (fun _ -> dispatch (PickupNewIcon iconType))
        ]
    let spawnerListTemplate
        (sectionName : string)
        typeConstructor
        iconSource =
        let spawnerList =
            List.map
                (fun text -> Html.li [ spawnerView text (typeConstructor text) dispatch ])
        Html.div [
            Html.h4 sectionName
            Html.ul [
                prop.children (spawnerList iconSource)
            ]
        ]
    let defaultIconSpawners =
        Html.div [
            prop.id "default-icon-spawners"
            prop.children [
                spawnerListTemplate
                    "Unary"
                    (fun op -> BaseUnaryIcon op)
                    defaultUnaryOperators
                spawnerListTemplate
                    "Binary"
                    (fun op -> BaseBinaryIcon op)
                    defaultBinaryOperators
                spawnerListTemplate
                    "Special"
                    (fun _ -> BaseIfIcon)
                    [ "If" ]
            ]
        ]
    defaultIconSpawners

let private customOperationSpawnersView (state : State) (dispatch : Message -> unit) : ReactElement =
    let customOperationSpawnerView (iconTypeName : string) (iconType : CustomOperation) =
        Html.div [
            prop.id "custom-icon-spawners"
            prop.children [
                Html.button [
                    prop.text iconTypeName
                    if not (CustomOperationNameContainsInvalidCharacter iconTypeName) then
                        prop.onClick (fun _ -> dispatch (PickupNewIcon (CustomOperation (iconTypeName, iconType.ParameterCount))))
                ]
                Html.button [
                    prop.text "Edit"
                    prop.onClick (fun _ -> dispatch (EditCustomOperation iconTypeName))
                ]
            ]
        ]
    let customOperationsSpawners =
        Html.div ( state.CustomOperations
            |> Map.toList
            |> List.filter (fun (name, _) -> name <> dummyCustomOperationName)
            |> List.map
                (fun (id, icon) ->
                    let name = CustomOperationNameRemoveInvisiblePart id
                    customOperationSpawnerView name icon) )
    customOperationsSpawners
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

let private customOperationCreatorView (state : State) (dispatch : Message -> unit) : ReactElement =
    let iconNameInput =
        Html.input [
            prop.className "custom-icon-creator-name-input"
            prop.type' "text"
            prop.placeholder "Operation Name"
            prop.onTextChange (fun newText ->
                dispatch (ChangeCustomOperationCreatorName newText) )
        ]
    let iconParamCountInput =
        Html.input [
            prop.className "custom-icon-creator-param-count-input"
            prop.type' "number"
            prop.placeholder "Parameter Count"
            prop.onTextChange (fun newText ->
                dispatch (ChangeCustomOperationCreatorParameterCount newText) )
        ]
    let createNewIconButton =
        let isInputValid = isText state.CustomOperationCreatorName && isNumber state.CustomOperationCreatorParameterCount
        Html.button [
            prop.disabled (not isInputValid)
            prop.text "New Icon"
            prop.onClick (fun _ ->
                dispatch (CreateCustomOperation(state.CustomOperationCreatorName, int state.CustomOperationCreatorParameterCount))
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

let tabParametersView (state : State) (dispatch : Message -> unit) : ReactElement =
    let drawParameter index (parameter : int) =
        let parameterText =
            sprintf "%d" parameter
        Html.div [
            prop.className "tab-parameter"
            prop.text parameterText
            prop.onClick (fun _ -> dispatch (PickupIconParameter (OperationParameter index)))
        ]
    let parameters =
        Html.div [
            prop.id "tab-parameters"
            prop.children
                ( getCurrentTab state
                |> fun tab -> tab.MasterCustomOperationParameters
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
            prop.children (renderIcons state dispatch)
        ]
    canvas

let render (state : State) (dispatch : Message -> unit) : ReactElement =
    Html.div [
        prop.id "root-container"
        prop.children [
            heldObjectView state
            tabParametersView state dispatch
            defaultIconSpawnersView dispatch
            customOperationSpawnersView state dispatch
            renderIconCanvas state dispatch
            customOperationCreatorView state dispatch
            constantSpawnerView state dispatch
            tabView state dispatch
        ]

        prop.onContextMenu (fun e ->
            if stateIsHoldingObject state then
                e.preventDefault()
                dispatch CancelPickup )
    ]

