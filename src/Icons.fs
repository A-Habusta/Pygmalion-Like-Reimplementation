module PygmalionReimplementation.Icons

open System

type CustomIconName = string
type IconID = Guid

type SimpleInstruction =
    | Trap
    | Constant of int
    | BaseIconParameter of int
    | LocalIconReference of IconID

type TopLevelInstruction =
    | TopLevelTrap
    | Unary of string * SimpleInstruction
    | Binary of string * SimpleInstruction * SimpleInstruction
    | If of SimpleInstruction * SimpleInstruction * SimpleInstruction
    | CallCustomIcon of CustomIconName * SimpleInstruction list

type LocalIconCollection = Map<IconID, TopLevelInstruction>

type CustomIconType =
    { MainIconID : IconID
      ParameterCount : int
      LocalIcons : LocalIconCollection }

type CustomIconTypesMap = Map<CustomIconName, CustomIconType>

let fetchLocalIcon (iconID : IconID) (iconCollection : LocalIconCollection) =
    match iconCollection.TryGetValue iconID with
    | true, instruction -> Some instruction
    | false, _ -> None

let saveLocalIcon (iconID : IconID) (iconCollection : LocalIconCollection) (instruction : TopLevelInstruction) =
    iconCollection.Add(iconID, instruction)

let removeLocalIcon (iconID : IconID) (iconCollection : LocalIconCollection) =
    iconCollection.Remove(iconID)