module SoftwareProject.Main

open Eval
open System


let run input =
    failwith "Not implemented"

let rec readConsole stop =
    let input = Console.ReadLine()
    runCommand input

and runCommand input =
    match input with
    | "exit" -> ()
    | _ ->
        let result = run input
        printfn "%A" result
        readConsole false