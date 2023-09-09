module SoftwareProject.Main

open Eval
open Utils

let runCommand input =
    failwith "Not implemented"

let rec readConsole stop =
    let input = Console.ReadLine()
    runCommand input
    match stop with
    | true -> ()
    | false -> readConsole stop