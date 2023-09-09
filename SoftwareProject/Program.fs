module SoftwareProject.Main


open System
open Icons
open Eval


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

let testedInstructionTree =
    Binary("+", Primitive(1), Primitive(1))

let emptyContext = {
    TypeLibrary = Map.empty
    ID = Guid.Empty
    EvaluatedParams = List.Empty
}


let testInstructionTree =
    let result = eval emptyContext testedInstructionTree
    printfn "%A" result

[<EntryPoint>]
let main argv =
    testInstructionTree
    0