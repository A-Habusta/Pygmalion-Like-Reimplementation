module SoftwareProject.Factorial

open System
open Icons
open Eval


let factorialInstructionTree =
    let recurse = IconCall("factorial", [withEmptyID (Binary("-", withEmptyID (BaseIconParameter(0)), withEmptyID (Constant(1))))])
    let condition = Binary("=", withEmptyID (BaseIconParameter(0)), withEmptyID (Constant(0)))
    let trueBranch = Constant(1)
    let falseBranch = Binary("*", withEmptyID (BaseIconParameter(0)), withEmptyID recurse)
    withEmptyID (If(withEmptyID condition, withEmptyID trueBranch, withEmptyID falseBranch))

let factorialIconType = {
    InstructionTree = factorialInstructionTree
    ParameterCount = 1
}

let typeLibrary = Map<IconTypeName, IconType>[(IconTypeName("factorial"), factorialIconType)]

let factorialContextTemplate = {
    TypeLibrary = typeLibrary
    EvaluatedParams = []
    ID = Guid.Empty
}

let iconFactorial number =
    let context = { factorialContextTemplate with EvaluatedParams = [number] }
    eval context factorialInstructionTree

let referenceFactorial number =
    let rec factorialInternal number counter =
        match number with
        | 0 -> counter
        | _ -> factorialInternal (number - 1) (counter * number)

    factorialInternal number 1