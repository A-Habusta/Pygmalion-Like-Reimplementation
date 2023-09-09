module SoftwareProject.Icons

open System.Collections.Generic

type IconName = string

// Data representation
type Instruction =
    | Trap
    | Primitive of int
    | Unary of string * Instruction
    | Binary of string * Instruction * Instruction
    | If of Instruction * Instruction * Instruction
    | Icon of IconName * Instruction list
    | Parameter of int

type IconContext =
    { Parameters: int list }

type CustomIcon =
    { InstructionTree : Instruction
      ParameterCount : int
      IconContext : DrawnIcon list }

and DrawnIcon =
    { Name : string
      xPosition : int
      yPosition : int
      Result : int Option
      DrawnParameters : int Option list}

// Mutable type might be changed later
type IconLibrary =
    { Icons : Dictionary<IconName, CustomIcon> }

let iconLibrary = {Icons = Dictionary<IconName, CustomIcon>()}


let fetchIconInstructionTree library name =
    match library.Icons.TryGetValue(name) with
    | true, icon -> icon.InstructionTree
    | false, _ -> failwith "Icon not found"


let addIconToLibrary library name icon =
    library.Icons.Add(name, icon)