module SoftwareProject.Icons

open System

type IconTypeName = string
type IconID = Guid

// Data representation

type SpecificInstruction = IconID * Instruction
and Instruction =
    | Trap
    | Constant of int
    | Unary of string * SpecificInstruction
    | Binary of string * SpecificInstruction * SpecificInstruction
    | If of SpecificInstruction * SpecificInstruction * SpecificInstruction
    | IconCall of IconTypeName * SpecificInstruction array
    | Parameter of int

type IconInstance =
    { TypeName : IconTypeName
      Context : IconID array }

type IconType =
    { InstructionTree : SpecificInstruction
      ParameterCount : int }


type IconTypeLibrary = Map<IconTypeName, IconType>
type IconInstanceLibrary = Map<IconID, IconInstance>

let fetchIconFromTypeLibrary (library: IconTypeLibrary) name =
    match library.TryGetValue(name) with
    | true, icon -> icon
    | false, _ -> failwith $"Icon type {name} not found"

let addIconToTypeLibrary (library: IconTypeLibrary) name icon : IconTypeLibrary =
    library.Add(name, icon)

let fetchIconFromInstanceLibrary (library: IconInstanceLibrary) id =
    match library.TryGetValue(id) with
    | true, icon -> icon
    | false, _ -> failwith "Icon not found"

let addIconToInstanceLibrary (library: IconInstanceLibrary) id icon : IconInstanceLibrary =
    library.Add(id, icon)