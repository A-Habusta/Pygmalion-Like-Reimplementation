module PygmalionReimplementation.View

open System
open Feliz

open PygmalionReimplementation.State
open PygmalionReimplementation.Icons

let stateIsHoldingObject (state : State) =
    match state.HeldObject with
    | NoObject -> false
    | _ -> true

let renderIconDecorations (state : State) (iconID : IconID) (dispatch : Message -> unit) =
    let icon = getIconFromState state iconID
    let nothingHeld = not <| stateIsHoldingObject state
    let drawParam (index : int) (parameter : IconInstructionParameter) : ReactElement =
        let paramToString =
            match parameter with
            | LocalIconInstructionReference id ->
                let icon = getIconFromState state id
                match icon.Result with
                | Some result -> sprintf "%A" result
                | None -> "?"
            | BaseIconParameter position ->
                let func = state.MasterCustomIconParameters[position]
                match func.IsValueCreated with
                | true -> sprintf "%A" func.Value
                | false -> "?"
            | Constant value -> sprintf "%A" value
            | Trap -> String.Empty
        let writeable =
            match parameter with
            | LocalIconInstructionReference id ->
                let icon = getIconFromState state id
                icon.Result = None
            | _ -> true
        Html.input [
            prop.style [
                style.position.relative
                style.top (length.px 5)
                style.left (length.px (index * 10 + 5))
            ]
            prop.onContextMenu (fun e ->
                e.preventDefault()
                e.stopPropagation()
                dispatch (RemoveIconParameter (iconID, index)) )
            prop.onClick (fun e ->
                e.preventDefault()
                e.stopPropagation()
                if not nothingHeld then
                    dispatch (PlacePickup (IconParameter (iconID, index)))  )
            prop.type' "number"
            prop.placeholder paramToString
        ]
    Html.div ( extractInstructionParameters icon.IconInstruction
               |> List.mapi drawParam )


let renderIcon (state : State) (iconID : IconID) (dispatch : Message -> unit) : ReactElement =
    let icon = getIconFromState state iconID
    Html.div [
        prop.style [
            style.position.absolute
            style.left (length.px icon.X)
            style.top (length.px icon.Y)
            style.border (length.px 1, borderStyle.solid, "black")
        ]

        if not <| stateIsHoldingObject state then
            prop.onContextMenu (fun e ->
                e.preventDefault()
                e.stopPropagation()
                dispatch (RemoveIcon iconID) )
            prop.onClick (fun e ->
                dispatch (PickupIcon iconID) )
            prop.onMouseDown (fun e ->
                if e.button = 1 then
                    if icon.Result = None then
                        dispatch (EvaluateIcon iconID)
                    else
                        dispatch (PickupIconParameter (LocalIconInstructionReference iconID)))


        if icon.Result = None then
            prop.children [ (renderIconDecorations state iconID dispatch) ]
        else
            prop.text (sprintf "%A" icon.Result.Value)
    ]

let renderIconInstances (state : State) (dispatch : Message -> unit) : ReactElement list =
    getIconTableFromState state
    |> Map.toSeq
    |> Seq.map (fun (id, icon) -> renderIcon state id dispatch)
    |> Seq.toList

let defaultBinaryOperators =
    [ "+"; "-"; "*"; "/"; "%"; "="; "<>"; "<"; "<="; ">"; ">="; "&&"; "||"; ]
let defaultUnaryOperators =
    [ "+"; "-"; "!" ]

let renderSpawner (text : string) (iconType : IconType) (dispatch : Message -> unit) : ReactElement =
    Html.div [
        Html.button [
            prop.text text
            prop.onClick (fun _ -> dispatch (PickupNewIcon iconType))
        ]
    ]

let renderDefaultIconSpawners (dispatch : Message -> unit) : ReactElement =
    let spawnerList (typeConstructor : string -> IconType)=
        List.map
            (fun text -> Html.li [ renderSpawner text (typeConstructor text) dispatch ])

    Html.div [
        Html.h2 "Default Icons"
        Html.label [
            prop.text "Unary Operators"
            prop.children [
                Html.ul (
                    spawnerList (fun text -> BaseUnaryIcon text) defaultUnaryOperators
                )
            ]
        ]
        Html.label [
            prop.text "Unary Operators"
            prop.children [
                Html.ul (
                    spawnerList (fun text -> BaseUnaryIcon text) defaultUnaryOperators
                )
            ]
        ]
        Html.ul (
            spawnerList (fun text -> BaseBinaryIcon text) defaultBinaryOperators
        )
        Html.ul (
            [ Html.li [ renderSpawner "If" BaseIfIcon dispatch ] ]
        )
    ]

let renderCustomIconSpawners (state : State) (dispatch : Message -> unit) : ReactElement =
    let customIconSpawner (name : string) (iconType : CustomIconType) : ReactElement =
        Html.div [
            prop.className "custom-icon-spawner"
            prop.children [
                renderSpawner name (CustomIcon(name, iconType.ParameterCount)) dispatch
                Html.button [
                    prop.text "Edit"
                    prop.onClick (fun _ -> dispatch (NotImplemented "Editing custom icons is not yet implemented"))
                ]
            ]
        ]

    Html.div [
        Html.h2 "Custom Icons"
        Html.ul (
            state.CustomIcons
            |> Map.toSeq
            |> Seq.map (fun (name, iconType) -> customIconSpawner name iconType)
            |> Seq.toList
        )
    ]

let renderHeldObject (state : State) =
    let heldObjectToString =
        let object = state.HeldObject
        match object with
        | NewIcon iconType -> sprintf "%A" iconType
        | Parameter parameter ->
            match parameter with
            | LocalIconInstructionReference id ->
                let icon = getIconFromState state id
                match icon.Result with
                | Some result -> sprintf "%A" result
                | None -> "?"
            | BaseIconParameter position ->
                let func = state.MasterCustomIconParameters[position]
                match func.IsValueCreated with
                | true -> sprintf "%A" func.Value
                | false -> "?"
            | Constant value -> sprintf "%A" value
            | Trap -> "Trap"
        | ExistingIcon _ -> "Moving existing icon"
        | _ -> String.Empty

    Html.p [
        prop.text heldObjectToString
    ]

let render (state : State) (dispatch : Message -> unit) : ReactElement =
    Html.div [
        prop.style [
            style.display.grid
            style.gridTemplateAreas [
                ["default-icon-spawners"; "icon-canvas"; "custom-icon-spawners"]
                ["default-icon-spawners"; "toolbar"; "custom-icon-spawners"]
            ]
            style.gridTemplateColumns [ length.auto ; length.percent 80; length.auto ]
            style.gridTemplateRows [ length.percent 95; length.auto ]
            style.width (length.vw 100)
            style.height (length.vh 100)
            style.padding (length.px 0)
        ]
        prop.children [
            Html.div [
                prop.style [
                    style.gridArea "toolbar"
                    style.border (length.px 1, borderStyle.solid, "black")
                ]
                prop.id "toolbar"
                prop.children [ renderHeldObject state ]
            ]
            Html.div [
                prop.style [
                    style.gridArea "default-icon-spawners"
                    style.border (length.px 1, borderStyle.solid, "black")
                ]
                prop.id "default-icon-spawners"
                prop.children [ renderDefaultIconSpawners dispatch ]
            ]
            Html.div [
                prop.style [
                    style.gridArea "custom-icon-spawners"
                    style.border (length.px 1, borderStyle.solid, "black")
                ]
                prop.id "custom-icon-spawners"
                prop.children [ renderCustomIconSpawners state dispatch ]
            ]
            Html.div [
                prop.style [
                    style.gridArea "icon-canvas"
                    style.border (length.px 1, borderStyle.solid, "black")
                ]
                prop.id "icon-canvas"
                prop.children (renderIconInstances state dispatch)
                if stateIsHoldingObject state then
                    prop.onClick (fun e ->
                        dispatch (PlacePickup (Position (int e.clientX, int e.clientY))))
            ]
        ]

        if stateIsHoldingObject state then
            prop.onContextMenu (fun e ->
                e.preventDefault()
                dispatch CancelPickup )
    ]
