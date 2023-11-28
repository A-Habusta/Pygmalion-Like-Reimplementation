module PygmalionReimplementation.Icons

open System

type IconID = Guid

type IconInstructionParameter =
    | Trap
    | Constant of int
    | BaseIconParameter of int
    | LocalIconInstructionReference of IconID

type IconInstruction =
    | TopLevelTrap
    | Unary of operator : string * IconInstructionParameter
    | Binary of operator : string * IconInstructionParameter * IconInstructionParameter
    | If of IconInstructionParameter * IconInstructionParameter * IconInstructionParameter
    | CallCustomIcon of customIconName : string * IconInstructionParameter list

let newIconID () = Guid.NewGuid()

let extractInstructionParameters instruction =
    match instruction with
    | TopLevelTrap -> []
    | Unary(_, param) -> [ param ]
    | Binary(_, param1, param2) -> [ param1; param2 ]
    | If(arg, trueBranch, falseBranch) -> [ arg; trueBranch; falseBranch ]
    | CallCustomIcon (_, parameters) -> parameters
let saveParametersInInstruction (instruction : IconInstruction) (parameters: IconInstructionParameter list) =
    match instruction with
    | Unary(op, _) -> Unary(op, parameters[0])
    | Binary(op, _, _) -> Binary(op, parameters[0], parameters[1])
    | If _ -> If(parameters[0], parameters[1], parameters[2])
    | CallCustomIcon (iconType, _) -> CallCustomIcon(iconType, parameters)
    | _ -> instruction

let transformInstructionParameters transform (instruction : IconInstruction) =
    instruction
    |> extractInstructionParameters
    |> transform
    |> saveParametersInInstruction instruction

let replaceParameter position newParameter instruction =
    let transform =
        List.mapi (fun i param ->
            if i = position then newParameter
            else param )
    transformInstructionParameters transform instruction

type IconType =
    | BaseUnaryIcon of operation : string
    | BaseBinaryIcon of operation : string
    | BaseIfIcon
    | CustomIcon of customIconName : string * parameterCount : int

let createEmptyIconInstruction (iconType : IconType) =
    match iconType with
    | BaseUnaryIcon op -> Unary(op, Trap)
    | BaseBinaryIcon op -> Binary(op, Trap, Trap)
    | BaseIfIcon -> If(Trap, Trap, Trap)
    | CustomIcon(customIconName, paramCount) -> CallCustomIcon(customIconName, List.init paramCount (fun _ -> Trap))

type DrawnIcon =
    { X : int
      Y : int
      Result : int option
      IconType : IconType
      IconInstruction : IconInstruction }

type IconTable = Map<IconID, DrawnIcon>
let createDrawnIcon x y iconType =
    { X = x
      Y = y
      Result = None
      IconType = iconType
      IconInstruction = createEmptyIconInstruction iconType }

type CustomIconType =
    { ParameterCount : int
      SavedIcons : IconTable
      EntryPointIcon : IconID option}

type CustomIcons = Map<string, CustomIconType>