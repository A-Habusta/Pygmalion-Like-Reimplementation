﻿module PygmalionReimplementation.Main

open System
open Browser
open Elmish
open Elmish.React
open Feliz

open PygmalionReimplementation.Eval
open PygmalionReimplementation.Icons

type MovableObject =
    | NoObject
    | ExistingIcon of IconID
    | NewIcon of IconType
    | Parameter of IconInstructionParameter

type MovableObjectTarget =
    | Position of x : int * y : int
    | IconParameter of target : IconID * position : int

type State =
    { HeldObject : MovableObject
      CustomIcons : CustomIcons
      MasterCustomIconName : string
      MasterCustomIconParameters : Lazy<int> list }

type Message =
    | EvaluateIcon of IconID
    | CreateIcon of IconType
    | PickupIcon of IconID
    | PickupIconParameter of IconInstructionParameter
    | PlacePickup of MovableObjectTarget
    | CancelPickup
    | RemoveIcon of IconID
    | RemoveIconParameter of target : IconID * position : int
    | NotImplemented of message : string

let private getIconTableFromState (state : State) =
    state.CustomIcons[state.MasterCustomIconName].SavedIcons

let private stateWithNewIconTable (state : State) (newIconTable : IconTable) =
    { state with CustomIcons = state.CustomIcons |> Map.add state.MasterCustomIconName { state.CustomIcons[state.MasterCustomIconName] with SavedIcons = newIconTable } }

let private getIconFromState (state : State) (id : IconID) =
    getIconTableFromState state |> Map.find id

let private stateWithNewIcon (state : State) (id : IconID) (newIcon : DrawnIcon) =
    stateWithNewIconTable state (getIconTableFromState state |> Map.add id newIcon)

let private evalIconFromState (state : State) (id : IconID) =
    let icon = getIconFromState state id
    let context =
        { CustomIcons = state.CustomIcons
          ExecutingCustomIcon = state.CustomIcons[state.MasterCustomIconName]
          CurrentIconID = id
          Parameters = state.MasterCustomIconParameters }
    let newIcon = {icon with Result = Some (eval context icon.IconInstruction) }
    stateWithNewIcon state id newIcon

let removeIconReferences (currentIcon : DrawnIcon) (targetID : IconID) : DrawnIcon =
    let removeIconReferencesFromInstruction (parameters : IconInstructionParameter list) : IconInstructionParameter list =
        parameters
            |> List.map (fun param ->
                match param with
                | LocalIconInstructionReference id when id = targetID -> Trap
                | icon -> icon )

    { currentIcon with IconInstruction = transformInstructionParameters removeIconReferencesFromInstruction currentIcon.IconInstruction }


let dummyCustomIconName = "dummy"
let dummyCustomIcon : CustomIconType =
    { ParameterCount = 0
      SavedIcons = Map.empty
      EntryPointIcon = newIconID () }

let initialCustomIcons : CustomIcons =
    Map.empty |> Map.add dummyCustomIconName dummyCustomIcon

let init () : State =
    { HeldObject = NoObject
      CustomIcons = initialCustomIcons
      MasterCustomIconName = dummyCustomIconName
      MasterCustomIconParameters = [] }

let update (message : Message) (state : State) : State =
    match message with
    | EvaluateIcon id ->
        // TODO: Add error handling
        try
            evalIconFromState state id
        with TrapException e ->
            state

    | CreateIcon iconType ->
        {state with HeldObject = NewIcon iconType}
    | PickupIcon id ->
        {state with HeldObject = ExistingIcon id}
    | PickupIconParameter parameter ->
        {state with HeldObject = Parameter parameter}
    | CancelPickup ->
        {state with HeldObject = NoObject}
    | PlacePickup targetLocation ->
        match state.HeldObject with
        | NoObject -> failwith "Tried to place object without holding any"
        | NewIcon newIconType ->
            match targetLocation with
            | Position (x, y) ->
                createDrawnIcon x y newIconType
                |> stateWithNewIcon state (newIconID ())
            | _ -> failwith "Tried to place new icon on invalid target"
        | ExistingIcon iconID ->
            match targetLocation with
            | Position (x, y) ->
                let icon = getIconFromState state iconID
                let newIcon = { icon with X = x; Y = y }
                stateWithNewIcon state iconID newIcon
            | _ -> failwith "Tried to place existing icon on invalid target"
        | Parameter parameter ->
            match targetLocation with
            | IconParameter (targetID, position) ->
                let icon = getIconFromState state targetID
                let newIcon =
                    { icon with IconInstruction = replaceParameter position parameter icon.IconInstruction }
                stateWithNewIcon state targetID newIcon
            | _ -> failwith "Tried to place parameter on invalid target"
    | RemoveIcon id ->
        getIconTableFromState state
        |> Map.remove id
        |> Map.map (fun _ icon -> removeIconReferences icon id)
        |> stateWithNewIconTable state
    | RemoveIconParameter (targetID, position) ->
        let icon = getIconFromState state targetID
        let newIcon =
            { icon with IconInstruction = replaceParameter position Trap icon.IconInstruction }
        stateWithNewIcon state targetID newIcon
    | NotImplemented message ->
        printf "%s" message
        state

let stateIsHoldingObject (state : State) =
    match state.HeldObject with
    | NoObject -> false
    | _ -> true

let renderIcon (iconID : IconID) (icon : DrawnIcon) (dispatch : Message -> unit) : ReactElement =
    Html.div [
        prop.style [
            style.position.absolute
            style.left (length.px icon.X)
            style.top (length.px icon.Y)
            style.border (length.px 1, borderStyle.solid, "black")
            style.width (length.px 10)
            style.height (length.px 10)
        ]
        prop.onContextMenu (fun e ->
            e.preventDefault()
            dispatch (RemoveIcon iconID) )
    ]

let renderIconInstances (state : State) (dispatch : Message -> unit) : ReactElement list =
    getIconTableFromState state
    |> Map.toSeq
    |> Seq.map (fun (id, icon) -> renderIcon id icon dispatch)
    |> Seq.toList

let defaultBinaryOperators =
    [ "+"; "-"; "*"; "/"; "%"; "=="; "!="; "<"; "<="; ">"; ">="; "&&"; "||"; ]
let defaultUnaryOperators =
    [ "+"; "-"; "!" ]

let renderSpawner (text : string) (iconType : IconType) (dispatch : Message -> unit) : ReactElement =
    Html.div [
        Html.button [
            prop.text text
            prop.onClick (fun _ -> dispatch (CreateIcon iconType))
        ]
    ]

let renderDefaultIconSpawners (dispatch : Message -> unit) : ReactElement =
    let spawnerList (typeConstructor : string -> IconType)=
        List.map
            (fun text -> Html.li [ renderSpawner text (typeConstructor text) dispatch ])

    Html.div [
        Html.h2 "Default Icons"
        Html.ul (
            List.concat [
                spawnerList (fun text -> BaseUnaryIcon text) defaultUnaryOperators
                spawnerList (fun text -> BaseBinaryIcon text) defaultBinaryOperators
                [ Html.li [ renderSpawner "If" BaseIfIcon dispatch ] ]
            ]
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
            style.gridTemplateRows [ length.percent 95 ; length.auto ]
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
                prop.onClick (fun e ->
                    dispatch (PlacePickup (Position (int e.clientX, int e.clientY))))
            ]
        ]

        if stateIsHoldingObject state then
            prop.onContextMenu (fun e ->
                e.preventDefault()
                dispatch CancelPickup )
    ]

Program.mkSimple init update render
    |> Program.withReactSynchronous "root"
    |> Program.run