module SoftwareProject.Main

let testFunction name funcA funcB start stop =
    let sequenceA = seq { for i in start..stop -> funcA i }
    let sequenceB = seq { for i in start..stop -> funcB i }

    printfn ""
    printfn "Testing %s..." name

    // Ask if this is lazily evaluated
    let firstMismatch = Seq.zip sequenceA sequenceB |> Seq.tryFind (fun (a, b) -> a <> b)
    match firstMismatch with
    | Some item -> printfn "Test failed: Mismatch at item %A" item
    | None -> printfn "Test passed"

[<EntryPoint>]
let main argv =
    ignore argv
    0