module PygmalionReimplementation.View

open System
open Feliz

open PygmalionReimplementation.State
open PygmalionReimplementation.Icons

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
        let specialActionText =
            match getIconResultFromState state id with
            | Some _ -> "Get Reference"
            | None -> "Evaluate"
        let specialActionHandler (e : Browser.Types.MouseEvent) =
            mouseEventPreventPropagation e
            match getIconResultFromState state id with
            | Some _ -> PickupIconParameter (LocalIconInstructionReference id)
            | None -> EvaluateIcon id
            |> dispatch
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
                    prop.text specialActionText
                    prop.onClick specialActionHandler
                    prop.disabled (iconIsHeld state id)
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
          |> List.filter (fun (name, _) -> name <> getMasterCustomIconName state)
          |> List.map
            (fun (id, icon) ->
                customIconSpawnerView id icon) )

let private constantSpawnerView (state : State) (dispatch : Message -> unit) : ReactElement =
    let isNumber (text : string) =
        match Int32.TryParse text with
        | true, _ -> true
        | _ -> false

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

let private leftToolsView (state : State) (dispatch : Message -> unit) : ReactElement =
    Html.div [
        prop.style [
            style.margin (length.px 5)
        ]
        prop.children [
            defaultIconSpawnersView dispatch
            constantSpawnerView state dispatch
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
    let leftTools = "default-icon-spawners"
    let canvas = "icon-canvas"
    let tabs = "tabs"
    let rightTools= "custom-icon-spawners"

    let rootStyle = [
            style.display.grid
            style.gridTemplateAreas [
                [ leftTools; canvas; rightTools ]
                [ leftTools; tabs; rightTools ]
            ]
            style.gridTemplateColumns [ length.percent 10; length.percent 80; length.percent 10]
            style.gridTemplateRows [ length.vh 95; length.vh 5]
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
            gridArea leftTools (leftToolsView state dispatch)
            gridArea rightTools (customIconSpawnersView state dispatch)
            gridArea canvas (renderIconCanvas state dispatch)
            gridArea tabs (heldObjectView state)
        ]

        prop.onContextMenu (fun e ->
            if stateIsHoldingObject state then
                e.preventDefault()
                dispatch CancelPickup )
    ]

