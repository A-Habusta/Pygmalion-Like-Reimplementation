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
    | If of IconInstructionParameter
    | CallCustomIcon of customIconName : string * IconInstructionParameter list

let newIconID () = Guid.NewGuid()

let extractInstructionParameters instruction =
    match instruction with
    | TopLevelTrap -> []
    | Unary(_, param) -> [ param ]
    | Binary(_, param1, param2) -> [ param1; param2 ]
    | If(arg) -> [ arg; ]
    | CallCustomIcon (_, parameters) -> parameters
let saveParametersInInstruction (instruction : IconInstruction) (parameters: IconInstructionParameter list) =
    match instruction with
    | Unary(op, _) -> Unary(op, parameters[0])
    | Binary(op, _, _) -> Binary(op, parameters[0], parameters[1])
    | If _ -> If(parameters[0])
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
    | BaseIfIcon -> If(Trap)
    | CustomIcon(customIconName, paramCount) -> CallCustomIcon(customIconName, List.init paramCount (fun _ -> Trap))

type DrawnIcon =
    { X : int
      Y : int
      IconType : IconType
      IconInstruction : IconInstruction }

type IconTable = Map<IconID, DrawnIcon>
let createDrawnIcon x y iconType =
    { X = x
      Y = y
      IconType = iconType
      IconInstruction = createEmptyIconInstruction iconType }

type CustomIconType =
    { ParameterCount : int
      SavedIcons : IconTable
      EntryPointIcon : IconID option}

type CustomIcons = Map<string, CustomIconType>

type IconResultsTable = Map<IconID, int>

let InvalidCustomIconNameCharacters = "\t\n\r_;"
let CustomIconNameVisiblePartDivider = ";"

let CustomIconNameContainsInvalidCharacter (name : string) =
    name |> Seq.exists (fun c -> InvalidCustomIconNameCharacters.Contains(c))

let CustomIconNameRemoveInvisiblePart (name : string) =
    name.Split(CustomIconNameVisiblePartDivider)[0]

let DrawnIconVisibleIdCharacters = 4

let GetDrawnIconIdCharacters (iconId : IconID) (charCount : int) =
    iconId.ToString().Substring(0, charCount)

let private ifTrueCustomIconNameSuffix= "_true_"

let private ifFalseCustomIconNameSuffix= "_false_"

let private buildIfCustomIconName (parentCustomIconName : string) (id : IconID) (suffix : string) =
    let visibleId = GetDrawnIconIdCharacters id DrawnIconVisibleIdCharacters
    sprintf "%s%s%s" parentCustomIconName suffix visibleId

let getCustomIfIconNames (parentCustomIconName : string) (id : IconID) =
    let trueName = buildIfCustomIconName parentCustomIconName id ifTrueCustomIconNameSuffix
    let falseName = buildIfCustomIconName parentCustomIconName id ifFalseCustomIconNameSuffix
    (trueName, falseName)
