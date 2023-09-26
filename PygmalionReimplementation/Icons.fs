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

type LocalIconMap = Map<IconID, TopLevelInstruction>

type CustomIcon =
    { MainIconID : IconID
      ParameterCount : int
      LocalIcons : LocalIconMap }

type CustomIconMap = Map<CustomIconName, CustomIcon>