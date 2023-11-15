module PygmalionReimplementation.Main

open Elmish
open Elmish.React

open PygmalionReimplementation.State
open PygmalionReimplementation.View

Program.mkSimple init update render
    |> Program.withReactSynchronous "root"
    |> Program.run