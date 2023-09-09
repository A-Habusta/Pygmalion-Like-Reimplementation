module SoftwareProject.Factorial

open System
open Icons
open Eval

let factorialInstructionTree =
    let recurse = IconCall("factorial", Guid.Empty, [|Binary("-", Parameter(0), Constant(1))|])
    let condition = Binary("=", Parameter(0), Constant(0))
    let trueBranch = Constant(1)
    let falseBranch = Binary("*", Parameter(0), recurse)
    If(condition, trueBranch, falseBranch)

let factorialIconType = {
    InstructionTree = factorialInstructionTree
    ParameterCount = 1
}

let typeLibrary = Map<IconTypeName, IconType>[(IconTypeName("factorial"), factorialIconType)]

let factorialContextTemplate = {
    TypeLibrary = typeLibrary
    EvaluatedParams = [||]
    ID = Guid.Empty
}

let iconFactorial number =
    let context = { factorialContextTemplate with EvaluatedParams = [| number |] }
    eval context factorialInstructionTree

let referenceFactorial number =
    let rec factorialInternal number counter =
        match number with
        | 0 -> counter
        | _ -> factorialInternal (number - 1) (counter * number)

    factorialInternal number 1