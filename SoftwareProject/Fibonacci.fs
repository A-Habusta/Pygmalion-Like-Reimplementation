module SoftwareProject.Fibonacci

open System
open Icons
open Eval

let fibonacciInstructionTree =
    withEmptyID (IconCall("fibonacciInternal", [| withEmptyID (BaseIconParameter(0)); withEmptyID (Constant(1)); withEmptyID (Constant(1)) |]))

// You don't have to do this, it's just a lot faster than using basic recursion
let fibonacciInternalInstructionTree =
    let addParameters = Binary("+", withEmptyID (BaseIconParameter(1)), withEmptyID (BaseIconParameter(2)))
    let decrementCounter = Binary("-", withEmptyID (BaseIconParameter(0)), withEmptyID (Constant(1)))
    withEmptyID(
        If ( withEmptyID (Binary("=", withEmptyID (BaseIconParameter(0)), withEmptyID (Constant(0)))),
        withEmptyID (BaseIconParameter(1)),
        withEmptyID (IconCall("fibonacciInternal", [| withEmptyID decrementCounter; withEmptyID (BaseIconParameter(2)); withEmptyID addParameters |])) ))

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