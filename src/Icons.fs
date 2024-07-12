module PygmalionReimplementation.Icons

open System

type IconID = Guid

type IconOperationParameter =
    | Trap
    | Constant of int
    | OperationParameter of int
    | LocalIconReference of IconID

type IconOperation =
    | TopLevelTrap
    | Unary of operator : string * IconOperationParameter
    | Binary of operator : string * IconOperationParameter * IconOperationParameter
    | If of IconOperationParameter
    | CallCustomOperation of customOperationName : string * IconOperationParameter list

let newIconID () = Guid.NewGuid()

let extractOperationParameters operation =
    match operation with
    | TopLevelTrap -> []
    | Unary(_, param) -> [ param ]
    | Binary(_, param1, param2) -> [ param1; param2 ]
    | If(arg) -> [ arg; ]
    | CallCustomOperation (_, parameters) -> parameters
let saveParametersInOperation (operation : IconOperation) (parameters: IconOperationParameter list) =
    match operation with
    | Unary(op, _) -> Unary(op, parameters[0])
    | Binary(op, _, _) -> Binary(op, parameters[0], parameters[1])
    | If _ -> If(parameters[0])
    | CallCustomOperation (iconType, _) -> CallCustomOperation(iconType, parameters)
    | _ -> operation

let transformOperationParameters transform (operation : IconOperation) =
    operation
    |> extractOperationParameters
    |> transform
    |> saveParametersInOperation operation

let replaceParameter position newParameter operation =
    let transform =
        List.mapi (fun i param ->
            if i = position then newParameter
            else param )
    transformOperationParameters transform operation

type IconType =
    | BaseUnaryIcon of operation : string
    | BaseBinaryIcon of operation : string
    | BaseIfIcon
    | CustomOperation of customOperationName : string * parameterCount : int

let createEmptyIconOperation (iconType : IconType) =
    match iconType with
    | BaseUnaryIcon op -> Unary(op, Trap)
    | BaseBinaryIcon op -> Binary(op, Trap, Trap)
    | BaseIfIcon -> If(Trap)
    | CustomOperation(customOperationName, paramCount) -> CallCustomOperation(customOperationName, List.init paramCount (fun _ -> Trap))

type Icon =
    { X : int
      Y : int
      IconType : IconType
      Operation : IconOperation }

type IconTable = Map<IconID, Icon>
let createIcon x y iconType =
    { X = x
      Y = y
      IconType = iconType
      Operation = createEmptyIconOperation iconType }

type CustomOperation =
    { ParameterCount : int
      SavedIcons : IconTable
      EntryPointIcon : IconID option}

type CustomOperations = Map<string, CustomOperation>

type IconResultsTable = Map<IconID, int>

let InvalidCustomOperationNameCharacters = "\t\n\r_;"
let CustomOperationNameVisiblePartDivider = ";"

let CustomOperationNameContainsInvalidCharacter (name : string) =
    name |> Seq.exists (fun c -> InvalidCustomOperationNameCharacters.Contains(c))

let CustomOperationNameRemoveInvisiblePart (name : string) =
    name.Split(CustomOperationNameVisiblePartDivider)[0]

let IconVisibleIdCharacters = 4

let GetIconIdCharacters (iconId : IconID) (charCount : int) =
    iconId.ToString().Substring(0, charCount)

let private ifTrueCustomOperationNameSuffix= "_true_"

let private ifFalseCustomOperationNameSuffix= "_false_"

let private buildIfCustomOperationName (parentCustomOperationName : string) (id : IconID) (suffix : string) =
    let visibleId = GetIconIdCharacters id IconVisibleIdCharacters
    sprintf "%s%s%s" parentCustomOperationName suffix visibleId

let getCustomIfIconNames (parentCustomOperationName : string) (id : IconID) =
    let trueName = buildIfCustomOperationName parentCustomOperationName id ifTrueCustomOperationNameSuffix
    let falseName = buildIfCustomOperationName parentCustomOperationName id ifFalseCustomOperationNameSuffix
    (trueName, falseName)
