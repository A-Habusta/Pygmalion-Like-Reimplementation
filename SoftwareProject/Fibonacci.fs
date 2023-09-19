module SoftwareProject.Fibonacci

open System
open Icons
open Eval

let fibonacciInstructionTree =
    IconCall("fibonacciInternal", Guid.Empty, [| Parameter(0); Constant(1); Constant(1) |])

// You don't have to do this, it's just a lot faster than using basic recursion
let fibonacciInternalInstructionTree =
    let addParameters = Binary("+", BaseIconParameter(1), BaseIconParameter(2))
    let decrementCounter = Binary("-", BaseIconParameter(0), Constant(1))
    If( Binary("=", Parameter(0), Constant(0)),
        BaseIconParameter(1),
        IconCall("fibonacciInternal", Guid.Empty, [| decrementCounter; Parameter(2); addParameters |]) )

let fibonacciIconType = {
    InstructionTree = fibonacciInstructionTree
    ParameterCount = 1
}

let internalFibonacciIconType = {
    InstructionTree = fibonacciInternalInstructionTree
    ParameterCount = 3
}

let typeLibrary = Map<IconTypeName, IconType> [
    (IconTypeName("fibonacci"), fibonacciIconType)
    (IconTypeName("fibonacciInternal"), internalFibonacciIconType) ]

let fibonacciContextTemplate = {
    TypeLibrary = typeLibrary
    EvaluatedParams = [||]
    ID = Guid.Empty
}

let iconFibonacci number =
    let context = { fibonacciContextTemplate with EvaluatedParams = [| number |] }
    eval context fibonacciInstructionTree

let referenceFibonacci number =
    let rec fibonacciInternal number counter1 counter2 =
        if number = 0 then counter1
        else fibonacciInternal (number - 1) counter2 (counter1 + counter2)

    fibonacciInternal number 1 1