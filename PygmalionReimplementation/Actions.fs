module PygmalionReimplementation.Actions

open PygmalionReimplementation.Icons

type ParameterPosition = int

let extractInstructionParameters instruction =
    match instruction with
    | TopLevelTrap -> []
    | Unary(_, param) -> [ param ]
    | Binary(_, param1, param2) -> [ param1; param2 ]
    | If(arg, trueBranch, falseBranch) -> [ arg; trueBranch; falseBranch ]
    | CallCustomIcon (_, parameters) -> parameters

let saveParametersInInstruction (instruction : TopLevelInstruction) (parameters: SimpleInstruction list) =
    match instruction with
    | Unary(op, _) -> Unary(op, parameters[0])
    | Binary(op, _, _) -> Binary(op, parameters[0], parameters[1])
    | If _ -> If(parameters[0], parameters[1], parameters[2])
    | CallCustomIcon (iconType, _) -> CallCustomIcon(iconType, parameters)
    | _ -> instruction

let transformInstructionParameters transform (instruction : TopLevelInstruction) =
    instruction
    |> extractInstructionParameters
    |> transform
    |> saveParametersInInstruction instruction

let replaceParameter (position : ParameterPosition) newParameter instruction =
    let transform =
        List.mapi (fun i param ->
            if i = position then newParameter
            else param )
    transformInstructionParameters transform instruction

let addParameterToLocalIcon localIconID position newParameter (localIcons : LocalIconMap) : LocalIconMap =
    localIcons[localIconID]
    |> replaceParameter position newParameter
    |> fun newInstruction -> localIcons.Add (localIconID, newInstruction)

let removeParameterFromLocalIcon localIconId position (localIcons : LocalIconMap) : LocalIconMap =
    addParameterToLocalIcon localIconId position Trap localIcons

let removeLocalIcon localIconID (localIcons : LocalIconMap) : LocalIconMap =
    let removeLocalIconCallsFromParameters localIconID (instruction : TopLevelInstruction) =
        let transform =
            List.map (fun param ->
                match param with
                | LocalIconReference iconID when iconID = localIconID -> Trap
                | _ -> param )
        transformInstructionParameters transform instruction

    localIcons.Remove localIconID
    |> Map.map (fun _ -> removeLocalIconCallsFromParameters localIconID)

type IconType =
    | BaseUnaryIcon of string
    | BaseBinaryIcon of string
    | BaseIfIcon
    | CustomIcon of CustomIconName * int

let createIcon (iconType : IconType) =
    match iconType with
    | BaseUnaryIcon op -> Unary(op, Trap)
    | BaseBinaryIcon op -> Binary(op, Trap, Trap)
    | BaseIfIcon -> If(Trap, Trap, Trap)
    | CustomIcon(customIconName, paramCount) -> CallCustomIcon(customIconName, List.init paramCount (fun _ -> Trap))

type Action =
    | ReplaceParameter of IconID * ParameterPosition * SimpleInstruction
    | RemoveParameter of IconID * ParameterPosition
    | AddIcon of IconID * IconType
    | RemoveIcon of IconID

let applyAction (action : Action) (localIcons : LocalIconMap) : LocalIconMap =
    match action with
    | ReplaceParameter (iconID, position, newParameter) -> addParameterToLocalIcon iconID position newParameter localIcons
    | RemoveParameter (iconID, position) -> removeParameterFromLocalIcon iconID position localIcons
    | AddIcon (iconID, iconType) -> localIcons.Add (iconID, createIcon iconType)
    | RemoveIcon iconID -> removeLocalIcon iconID localIcons

let applyActions (actions : Action list) (localIcons : LocalIconMap) : LocalIconMap =
    List.fold (fun localIcons action -> applyAction action localIcons) localIcons actions