module SoftwareProject.Factorial

open System
open Icons
open Eval


let factorialInstructionTree =
    let recurse = IconCall("factorial", [|toEmpty (Binary("-", toEmpty (BaseIconParameter(0)), toEmpty (Constant(1))))|])
    let condition = Binary("=", toEmpty (BaseIconParameter(0)), toEmpty (Constant(0)))
    let trueBranch = Constant(1)
    let falseBranch = Binary("*", toEmpty (BaseIconParameter(0)), toEmpty recurse)
    toEmpty (If(toEmpty condition, toEmpty trueBranch, toEmpty falseBranch))

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