module SoftwareProject.Fibonacci

open System
open Icons
open Eval

let fibonacciInstructionTree =
    toEmpty (IconCall("fibonacciInternal", [| toEmpty (BaseIconParameter(0)); toEmpty (Constant(1)); toEmpty (Constant(1)) |]))

// You don't have to do this, it's just a lot faster than using basic recursion
let fibonacciInternalInstructionTree =
    let addParameters = Binary("+", toEmpty (BaseIconParameter(1)), toEmpty (BaseIconParameter(2)))
    let decrementCounter = Binary("-", toEmpty (BaseIconParameter(0)), toEmpty (Constant(1)))
    toEmpty(
        If ( toEmpty (Binary("=", toEmpty (BaseIconParameter(0)), toEmpty (Constant(0)))),
        toEmpty (BaseIconParameter(1)),
        toEmpty (IconCall("fibonacciInternal", [| toEmpty decrementCounter; toEmpty (BaseIconParameter(2)); toEmpty addParameters |])) ))

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