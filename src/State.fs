module PygmalionReimplementation.State

open Aether
open Aether.Operators

open PygmalionReimplementation.SimpleEval
open PygmalionReimplementation.Icons

type SingleTabState =
    { TabName : string
      TabCustomIconPrism : CustomIconPrism
      TabParameters : UnderlyingNumberDataType list }

    static member TabName_ =
        _.TabName, (fun value a -> { a with TabName = value })
    static member TabCustomIconPrism_ =
        _.TabCustomIconPrism, (fun value a -> { a with TabCustomIconPrism = value })
    static member TabParameters_ =
        _.TabParameters, (fun value a -> { a with TabParameters = value })

type Tabs = SingleTabState list
type TabPrism = Prism<Tabs, SingleTabState>

type InputState =
    { ConstantSpawnerText : string
      CustomIconCreatorName : string
      CustomIconCreatorParameterCount : string }

    static member ConstantSpawnerText_ =
        _.ConstantSpawnerText, (fun value a -> { a with ConstantSpawnerText = value })
    static member CustomIconCreatorName_ =
        _.CustomIconCreatorName, (fun value a -> { a with CustomIconCreatorName = value })
    static member CustomIconCreatorParameterCount_ =
        _.CustomIconCreatorParameterCount, (fun value a -> { a with CustomIconCreatorParameterCount = value })

type State =
    { CustomIcons : CustomIcons
      CurrentLocalState : ExecutionState
      InputState : InputState }

    static member CustomIcons_ =
        _.CustomIcons, (fun value a -> { a with CustomIcons = value })
    static member CurrentLocalState_ =
        _.CurrentLocalState, (fun value a -> { a with CurrentLocalState = value })
    static member InputState_ =
        _.InputState, (fun value a -> { a with InputState = value })

type InputAction =
    | SetConstantSpawnerText of string
    | SetCustomIconCreatorName of string
    | SetCustomIconParameterCount of string
    | PressCustomIconCreatorButton
    | PressConstantSpawnerButton

type Action =
    | SwitchTab of TabPrism
    | InputAction of InputAction
    | IconAction of ExecutionActionTree