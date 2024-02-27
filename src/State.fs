module PygmalionReimplementation.State

open System

open PygmalionReimplementation.Utils
open PygmalionReimplementation.Icons

type SingleTabState =
    { TabName : string
      TabCustomIconPrism : CustomIconPrism
      TabParameters : UnderlyingNumberDataType list }

    static member TabName_ =
        (fun a -> a.TabName), (fun value a -> { a with TabName = value })
    static member TabCustomIconPrism_ =
        (fun a -> a.TabCustomIconPrism), (fun value a -> { a with TabCustomIconPrism = value })
    static member TabParameters_ =
        (fun a -> a.TabParameters), (fun value a -> { a with TabParameters = value })

type InputState =
    { ConstantSpawnerText : string
      CustomIconCreatorName : string
      CustomIconCreatorParameterCount : string }

    static member ConstantSpawnerText_ =
        (fun a -> a.ConstantSpawnerText), (fun value a -> { a with ConstantSpawnerText = value })
    static member CustomIconCreatorName_ =
        (fun a -> a.CustomIconCreatorName), (fun value a -> { a with CustomIconCreatorName = value })
    static member CustomIconCreatorParameterCount_ =
        (fun a -> a.CustomIconCreatorParameterCount), (fun value a -> { a with CustomIconCreatorParameterCount = value })

type State =
    { CustomIcons : CustomIcons
      CurrentLocalState : LocalState
      InputState : InputState }

    static member CustomIcons_ =
        (fun a -> a.CustomIcons), (fun value a -> { a with CustomIcons = value })
    static member CurrentLocalState_ =
        (fun a -> a.CurrentLocalState), (fun value a -> { a with CurrentLocalState = value })
    static member InputState_ =
        (fun a -> a.InputState), (fun value a -> { a with InputState = value })