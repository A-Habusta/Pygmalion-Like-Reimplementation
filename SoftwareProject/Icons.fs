module SoftwareProject.Icons

open System

type IconTypeName = string
type IconID = Guid

// Data representation
type Instruction =
    | Trap
    | Primitive of int
    | Unary of string * Instruction
    | Binary of string * Instruction * Instruction
    | If of Instruction * Instruction * Instruction
    | Icon of IconTypeName * IconID * Instruction list
    | Parameter of int

type IconInstance =
    { TypeName : IconTypeName
      Context : IconID list }

type IconType = {
    InstructionTree : Instruction
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


let iconInstanceLibrary = Map<IconID, IconInstance>[]