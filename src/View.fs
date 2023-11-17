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
        let icon = getIconFromState state id
        match icon.Result with
        | Some result -> sprintf "%A" result
        | None -> unknownIdentifier
    | BaseIconParameter position ->
        let func = state.MasterCustomIconParameters[position]
        match func.IsValueCreated with
        | true -> sprintf "%A" func.Value
        | false -> unknownIdentifier
    | Constant value -> sprintf "%A" value
    | Trap -> "Trap"


let private renderIcon
    (icon : DrawnIcon)
    (id : IconID)
    (state : State)
    (dispatch : Message -> unit) : ReactElement =
    let renderIconIOField =
        let renderParameter (parameter : IconInstructionParameter) =
            let parameterText = parameterToString state parameter
            Html.div [
                prop.text parameterText
            ]
        Html.div (
            List.map renderParameter (extractInstructionParameters icon.IconInstruction)
        )
    let renderIconActions =
        let specialActionText =
            match icon.Result with
            | Some _ -> "Pickup Reference"
            | None -> "Evaluate"
        let specialActionHandler (e : Browser.Types.MouseEvent) =
            mouseEventPreventPropagation e
            match icon.Result with
            | Some _ -> PickupIconParameter (LocalIconInstructionReference id)
            | None -> EvaluateIcon id
            |> dispatch
        Html.div [
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
    Html.div [
        prop.style [
            style.position.absolute
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
          |> List.map
            (fun (id, icon) ->
                customIconSpawnerView id icon) )

let private heldObjectView (state : State) =
    let heldObjectToString =
        let object = state.HeldObject
        match object with
        | NewIcon iconType -> sprintf "%A" iconType
        | Parameter parameter ->
            parameterToString state parameter
        | ExistingIcon _ -> "Moving existing icon"
        | _ -> String.Empty

    Html.p [
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
            style.gridTemplateColumns [ length.auto ; length.percent 80; length.auto ]
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
            gridArea leftTools (defaultIconSpawnersView dispatch)
            gridArea rightTools (customIconSpawnersView state dispatch)
            gridArea canvas (renderIconCanvas state dispatch)
            gridArea tabs (heldObjectView state)
        ]

        prop.onContextMenu (fun e ->
            if stateIsHoldingObject state then
                e.preventDefault()
                dispatch CancelPickup )
    ]
