module PygmalionReimplementation.Main

open System
open Fable
open Browser
open Elmish
open Fable.React
open Elmish.React

open PygmalionReimplementation.Eval
open PygmalionReimplementation.Icons

type State =
    { HeldIcon : Option<IconID>
      HeldParameter : Option<IconInstructionParameter> 
      CustomIcons : CustomIcons
      MasterCustomIconName : string
      MasterCustomIconParameters : int list }

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

let render (state : State) (dispatch : Message -> unit) : ReactElement =
    failwith "not implemented"


Program.mkSimple init update render
    |> Program.withReactSynchronous "elmish-app"
    |> Program.run