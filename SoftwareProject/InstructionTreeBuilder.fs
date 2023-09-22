module SoftwareProject.InstructionTreeBuilder

open Utils
open Icons

type InstructionTreeOperation =
    | Add of IconID * ParameterPosition * SpecificInstruction
    | Remove of IconID

and ParameterPosition = int

let applyOperation fAdd fRemove (operation : InstructionTreeOperation) (tree : SpecificInstruction) =
    match operation with
    | Add(parentIconID, pos, instruction) -> fAdd parentIconID pos instruction tree
    | Remove iconID -> fRemove iconID tree

let getInstructionParameterCount (iconTypeLibrary : IconTypeLibrary) instruction =
    match instruction with
    | Unary _ -> 1
    | Binary _ -> 2
    | If _ -> 3
    | IconCall(typeName, _) ->
        match iconTypeLibrary.TryGetValue typeName with
        | false, _ -> 0
        | true, out -> out.ParameterCount
    | _ -> 0

let extractInstructionParameters instruction =
    match instruction with
    | Unary(_, param) -> [ param ]
    | Binary(_, param1, param2) -> [ param1; param2 ]
    | If(arg, trueBranch, falseBranch) -> [ arg; trueBranch; falseBranch ]
    | IconCall(_, parameters) -> parameters
    | _ -> []

let saveParametersInInstruction (instruction : Instruction) (parameters: SpecificInstruction list) =
    match instruction with
    | Unary(op, _) -> Unary(op, parameters[0])
    | Binary(op, _, _) -> Binary(op, parameters[0], parameters[1])
    | If _ -> If(parameters[0], parameters[1], parameters[2])
    | IconCall(iconType, _) -> IconCall(iconType, parameters)
    | _ -> instruction

let replaceParameter position newParameter instruction =
    instruction
    |> extractInstructionParameters
    |> replaceAt position newParameter
    |> saveParametersInInstruction instruction


let modifyInstructionInTree fModify (targetID : IconID) (root : SpecificInstruction) =
    let rec modifyInstructionInTreeRec ((id, currentInstruction) : SpecificInstruction) =
        if id = targetID then
            fModify (id, currentInstruction)
        else
            let newChildren = currentInstruction |> extractInstructionParameters |> List.map modifyInstructionInTreeRec
            (id, saveParametersInInstruction currentInstruction newChildren)

    modifyInstructionInTreeRec root

let addInstruction
    (parentID : IconID) (position : ParameterPosition) (newInstruction : SpecificInstruction) (root : SpecificInstruction) =

    let replaceParameterInSpecificInstruction ((id, instruction) : SpecificInstruction) =
        (id, replaceParameter position newInstruction instruction)

    modifyInstructionInTree replaceParameterInSpecificInstruction parentID root

let removeInstruction (targetID : IconID) (root : SpecificInstruction) =
    modifyInstructionInTree (fun _ -> DefaultTrapInstruction) targetID root