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

let defaultBorder = style.border (length.px 1, borderStyle.solid, "black")

let private stateIsHoldingObject (state : State) =
    match state.HeldObject with
    | NoObject -> false
    | _ -> true

let iconIsHeld (state : State) (id : IconID) =
    match state.HeldObject with
    | ExistingIcon heldId -> heldId = id
    | _ -> false

let private mouseEventPreventPropagation (e : Browser.Types.MouseEvent) =
    e.preventDefault()
    e.stopPropagation()

let private parameterToString (state : State) (parameter : IconInstructionParameter) =
    match parameter with
    | LocalIconInstructionReference id ->
        match getIconResultFromState state id with
        | Some result -> sprintf "%A" result
        | None -> unknownIdentifier
    | BaseIconParameter position ->
        let func = getMasterCustomIconParameters state |> List.item position
        match func.IsValueCreated with
        | true -> sprintf "%A" func.Value
        | false -> unknownIdentifier
    | Constant value -> sprintf "%d" value
    | Trap -> String.Empty

let private renderIcon
    (icon : DrawnIcon)
    (id : IconID)
    (state : State)
    (dispatch : Message -> unit) : ReactElement =
    let renderIconIOField =
        let renderParameter (index : int) (parameter : IconInstructionParameter) =
            let parameterBoxSize = 30
            let parameterStyle = [
                style.minWidth (length.px parameterBoxSize)
                style.height (length.px parameterBoxSize)
                style.margin (length.px 2)
                style.display.flex
                defaultBorder
            ]
            let deleteButton =
                Html.div [
                    prop.text "X"
                    prop.onClick (fun e ->
                        mouseEventPreventPropagation e
                        dispatch (RemoveIconParameter(id, index)) )
                    prop.style [
                        style.fontSize (length.px 6)
                        style.position.absolute
                    ]
                ]
            let text =
                Html.div [
                    prop.text (parameterToString state parameter)
                    prop.style [
                        style.alignSelf.center
                    ]
                ]

            Html.div [
                prop.style parameterStyle
                prop.onClick (fun e ->
                    mouseEventPreventPropagation e
                    dispatch (PlacePickup(IconParameter(id, index))) )
                prop.children [
                    text
                    if parameter <> Trap then
                        deleteButton
                ]
            ]

        let decorateIcon (icon : DrawnIcon) (renderedParameters : ReactElement list) =
            let iconDecoratorText (text : string) =
                let boldWeight = 700
                Html.div [
                    prop.text text
                    prop.style [
                        style.margin (length.px 5)
                        style.fontWeight boldWeight
                    ]
                ]

            match icon.IconInstruction with
            | Unary (op, _) ->
                [ iconDecoratorText op; renderedParameters[0] ]
            | Binary (op, _, _) ->
                [ renderedParameters[0]; iconDecoratorText op; renderedParameters[1] ]
            | If (_, _, _) -> [
                    iconDecoratorText "If"
                    renderedParameters[0]
                    iconDecoratorText "Then"
                    renderedParameters[1]
                    iconDecoratorText "Else"
                    renderedParameters[2]
                ]
            | CallCustomIcon (name, _) ->
                iconDecoratorText name :: renderedParameters
            | TopLevelTrap -> [ Html.text "Trap" ]

        match getIconResultFromState state id with
        | Some result ->
            Html.div [
                prop.style [
                    style.margin (length.px 5)
                ]
                prop.text (sprintf "%d" result)
            ]
        | None ->
            Html.div [
                prop.style [
                    style.display.flex
                    style.alignContent.center
                ]
                prop.children (
                    List.mapi renderParameter (extractInstructionParameters icon.IconInstruction)
                    |> decorateIcon icon )
            ]
    let renderIconActions =
        let getReferenceHandler =
            fun e ->
                dispatch (PickupIconParameter (LocalIconInstructionReference(id)))
        let evalHandler =
            fun e ->
                mouseEventPreventPropagation e
                dispatch (EvaluateIcon(id))

        let iconIsEvaluated = Option.isSome (getIconResultFromState state id)

        Html.div [
            prop.style [
                style.display.flex
            ]
            prop.children [
                Html.button [
                    prop.text "Remove"
                    prop.onClick (fun e ->
                        mouseEventPreventPropagation e
                        dispatch (RemoveIcon id) )
                    prop.disabled (iconIsHeld state id)
                ]
                Html.button [
                    prop.text "Get Reference"
                    prop.onClick getReferenceHandler
                    prop.disabled (iconIsHeld state id)
                ]
                Html.button [
                    prop.text "Eval"
                    prop.onClick evalHandler
                    prop.disabled (iconIsHeld state id || iconIsEvaluated)
                ]
                Html.button [
                    prop.text "Move"
                    prop.onClick (fun e ->
                        mouseEventPreventPropagation e
                        dispatch (PickupIcon id) )
                    prop.disabled (iconIsHeld state id)
                ]
            ]
        ]
    Html.div [
        prop.style [
            style.position.absolute
            style.display.flex
            style.left (length.px icon.X)
            style.top (length.px icon.Y)
            defaultBorder
        ]
        prop.children [
            renderIconIOField
            renderIconActions
        ]
    ]

let private renderIconInstances (state : State) (dispatch : Message -> unit) : ReactElement list =
    getIconTableFromState state
    |> Map.toList
    |> List.map (fun (id, icon) -> renderIcon icon id state dispatch)

let private defaultIconSpawnersView (dispatch : Message -> unit) : ReactElement =
    let spawnerView (text : string) (iconType : IconType) (dispatch : Message -> unit) : ReactElement =
        Html.button [
            prop.style [
                style.width (length.px 30)
            ]
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
    Html.div [
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

let private customIconSpawnersView (state : State) (dispatch : Message -> unit) : ReactElement =
    let customIconSpawnerView (iconTypeName : string) (iconType : CustomIconType) =
        Html.div [
            Html.button [
                prop.text iconTypeName
                prop.onClick (fun _ -> dispatch (PickupNewIcon (CustomIcon (iconTypeName, iconType.ParameterCount))))
            ]
            Html.button [
                prop.text "Edit"
                // TODO
                prop.onClick (fun _ -> dispatch (NotImplemented "Edit custom icon"))
            ]
        ]
    Html.div ( state.CustomIcons
          |> Map.toList
          |> List.filter (fun (name, _) -> name <> dummyCustomIconName)
          |> List.map
            (fun (id, icon) ->
                customIconSpawnerView id icon) )
let private constantSpawnerView (state : State) (dispatch : Message -> unit) : ReactElement =
    Html.div [
        prop.children [
            Html.input [
                prop.style [
                    style.width (length.percent 95)
                ]
                prop.type' "number"
                prop.placeholder "Constant"
                prop.onTextChange (fun newText ->
                    dispatch (ChangeConstantSpawnerText newText) )
            ]
            Html.button [
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

    Html.div [
        prop.text heldObjectToString
    ]

let private tabView (state : State) (dispatch : Message -> unit) : ReactElement =
    let tabViewStyle = [
        style.display.flex
        style.flexDirection.row
        style.height (length.perc 100)
        style.width (length.perc 100)
    ]

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
        Html.div [
            prop.style [
                style.marginRight (length.px 5)
            ]
            prop.children [
                button
                if not (isCurrentTab || isMainTab) then removeButton
            ]
        ]

    Html.div [
        prop.style tabViewStyle
        prop.children (
            state.Tabs
            |> List.mapi singleTabView
        )
    ]

let private customIconCreatorView (state : State) (dispatch : Message -> unit) : ReactElement =
    let iconNameInput =
        Html.input [
            prop.style [
                style.width (length.perc 90)
            ]
            prop.type' "text"
            prop.placeholder "Icon Name"
            prop.onTextChange (fun newText ->
                dispatch (ChangeCustomIconCreatorName newText) )
        ]
    let iconParamCountInput =
        Html.input [
            prop.style [
                style.width (length.perc 90)
            ]
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
    Html.div [
        prop.style [
            style.display.flex
            style.flexWrap.wrap
        ]

        prop.children [
            iconNameInput
            iconParamCountInput
            createNewIconButton
        ]
    ]

let customIconParametersView (state : State) (dispatch : Message -> unit) : ReactElement =
    let drawParameter index (parameter : Lazy<int>) =
        let parameterText =
            if parameter.IsValueCreated then
                sprintf "%d" parameter.Value
            else
                unknownIdentifier
        Html.div [
            prop.style [
                style.width (length.px 30)
                style.height (length.px 30)
                defaultBorder
                style.textAlign.center
            ]
            prop.text parameterText
            prop.onClick (fun _ -> dispatch (PickupIconParameter (BaseIconParameter index)))
        ]


    Html.div [
        prop.style [
            style.display.flex
            style.flexDirection.row
        ]

        prop.children
            ( getCurrentTab state
            |> fun tab -> tab.MasterCustomIconParameters
            |> List.mapi drawParameter )
    ]

let renderIconCanvas (state : State) (dispatch : Message -> unit) : ReactElement =
    let canvasOnClick (e : Browser.Types.MouseEvent) =
        mouseEventPreventPropagation e
        dispatch (PlacePickup (Position (int e.clientX, int e.clientY)))
    Html.div [
        prop.style [
            style.height (length.perc 100)
            style.width (length.perc 100)
        ]
        prop.onClick canvasOnClick
        prop.children (renderIconInstances state dispatch)
    ]

let render (state : State) (dispatch : Message -> unit) : ReactElement =
    let defaultIconPicker = "default-icon-spawners"
    let canvas = "icon-canvas"
    let tabs = "tabs"
    let rightTools= "custom-icon-spawners"
    let topBar = "top-bar"
    let customIconCreator = "customIconCreator"
    let constantSpawner = "constant-spawner"
    let heldObject = "held-object"

    let rootStyle = [
            style.display.grid
            style.gridTemplateAreas [
                [ defaultIconPicker; topBar; topBar; rightTools ]
                [ defaultIconPicker; canvas; canvas; rightTools ]
                [ constantSpawner; canvas; canvas; customIconCreator ]
                [ constantSpawner; heldObject; tabs; customIconCreator ]
            ]
            style.gridTemplateColumns [ length.vw 10; length.vw 10; length.vw 70; length.vw 10]
            style.gridTemplateRows [ length.vh 5; length.vh 83; length.vh 6; length.vh 6]
        ]
    let gridArea (areaName : string) (element : ReactElement) =
        Html.div [
            prop.style [
                style.gridArea areaName
                defaultBorder
            ]
            prop.children [ element ]
        ]
    Html.div [
        prop.style rootStyle
        prop.children [
            gridArea heldObject (heldObjectView state)
            gridArea topBar (customIconParametersView state dispatch)
            gridArea defaultIconPicker (defaultIconSpawnersView dispatch)
            gridArea rightTools (customIconSpawnersView state dispatch)
            gridArea canvas (renderIconCanvas state dispatch)
            gridArea customIconCreator (customIconCreatorView state dispatch)
            gridArea constantSpawner (constantSpawnerView state dispatch)
            gridArea tabs (tabView state dispatch)
        ]

        prop.onContextMenu (fun e ->
            if stateIsHoldingObject state then
                e.preventDefault()
                dispatch CancelPickup )
    ]

