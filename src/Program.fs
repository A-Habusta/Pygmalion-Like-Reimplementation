module PygmalionReimplementation.Main

open System
open Browser
open Elmish
open Elmish.React
open Feliz

open PygmalionReimplementation.Eval
open PygmalionReimplementation.Icons

type State =
    { HeldIcon : Option<IconID>
      HeldParameter : Option<IconInstructionParameter>
      CustomIcons : CustomIcons
      MasterCustomIconName : string
      MasterCustomIconParameters : Lazy<int> list }

type Message =
    | EvaluateIcon of IconID
    | AddNewIcon of IconType
    | RemoveIcon of IconID
    | RemoveIconParameter of target : IconID * position : int

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
    { HeldIcon = None
      HeldParameter = None
      CustomIcons = initialCustomIcons
      MasterCustomIconName = dummyCustomIconName
      MasterCustomIconParameters = [] }

let update (message : Message) (state : State) : State =
    match message with
    | EvaluateIcon id ->
        evalIconFromState state id
    | AddNewIcon iconType ->
        // Placeholder coordinates
        let newX = Random.Shared.Next(100, 500)
        let newY = Random.Shared.Next(100, 500)
        stateWithNewIcon state (newIconID ()) (createDrawnIcon newX newY iconType)
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

let renderIcon (icon : DrawnIcon) (dispatch : Message -> unit) : ReactElement =
    raise <| NotImplementedException()

let renderIconInstances (state : State) (dispatch : Message -> unit) : ReactElement list =
    getIconTableFromState state
    |> Map.toSeq
    |> Seq.map (fun (_, icon) -> renderIcon icon dispatch)
    |> Seq.toList

let defaultBinaryOperators =
    [ "+"; "-"; "*"; "/"; "%"; "=="; "!="; "<"; "<="; ">"; ">="; "&&"; "||"; ]
let defaultUnaryOperators =
    [ "+"; "-"; "!" ]

let renderSpawner (text : string) (iconType : IconType) (dispatch : Message -> unit) : ReactElement =
    Html.div [
        Html.button [
            prop.text text
            prop.onClick (fun _ -> dispatch (AddNewIcon iconType))
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
                Html.button [
                    prop.text name
                    prop.onClick (fun _ -> dispatch (AddNewIcon (CustomIcon(name, iconType.ParameterCount))))
                ]
                Html.button [
                    prop.text "Edit"
                    //prop.onClick ...
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

let render (state : State) (dispatch : Message -> unit) : ReactElement =
    Html.div [
        prop.style [ style.position.relative ]
        prop.children [
            Html.div [
                prop.id "default-icon-spawners"
                prop.children [ renderDefaultIconSpawners dispatch ]
            ]
            Html.div [
                prop.id "custom-icon-spawners"
                prop.children [ renderCustomIconSpawners state dispatch ]

            ]
            Html.div [
                prop.id "icon-canvas"
                prop.children (renderIconInstances state dispatch)
            ]
        ]
    ]

Program.mkSimple init update render
    |> Program.withReactSynchronous "elmish-app"
    |> Program.run