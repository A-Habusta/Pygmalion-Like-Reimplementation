module SoftwareProject.Main

open FactorialTest

(*
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
*)

let testFunction name funcA funcB start stop =
    let rec internalLoop i =
        if i < stop then
            let resultA = funcA i
            let resultB = funcB i
            if resultA <> resultB then
                printfn "Error: %A <> %A" resultA resultB
            internalLoop (i + 1)

    printfn "Testing %s..." name
    internalLoop start
    printfn "%s gives valid results from %d to %d" name start stop
    printfn ""


[<EntryPoint>]
let main argv =
    testFunction "Factorial" iconFactorial referenceFactorial 0 10
    0