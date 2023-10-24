module PygmalionReimplementation.Main

open System
open Fable
open Browser
open Elmish
open Elmish.React

open PygmalionReimplementation.Eval
open PygmalionReimplementation.Icons


type State = unit

let init () : State = ()

type Message =
    | Undo
    | Action of IconAction

let update (message : Message) (state : State) : State =
    raise <| NotImplementedException "update"