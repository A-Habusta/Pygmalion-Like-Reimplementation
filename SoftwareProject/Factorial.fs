module SoftwareProject.Factorial

open System
open SoftwareProject.Actions
open SoftwareProject.Icons
open SoftwareProject.Eval

let rec referenceFactorial n =
  if n = 0 then 1 else n * referenceFactorial (n - 1)

let recurseFactorialID = Guid.NewGuid ()
let subtractionID = Guid.NewGuid ()
let multiplicationID = Guid.NewGuid ()
let comparisonID = Guid.NewGuid ()
let ifID = Guid.NewGuid ()


let (actions : Action list) =
  [ AddIcon(ifID, BaseIfIcon)
    AddIcon(subtractionID, BaseBinaryIcon "-")
    AddIcon(comparisonID, BaseBinaryIcon "=")
    ReplaceParameter(subtractionID, 0, BaseIconParameter 0)
    ReplaceParameter(subtractionID, 1, Constant 1)
    ReplaceParameter(comparisonID, 0, BaseIconParameter 0)
    ReplaceParameter(comparisonID, 1, Constant 0)
    ReplaceParameter(ifID, 0, LocalIconReference comparisonID)
    ReplaceParameter(ifID, 1, Constant 1)
    AddIcon(multiplicationID, BaseBinaryIcon "*")
    AddIcon(recurseFactorialID, CustomIcon("factorial", 1))
    ReplaceParameter(ifID, 2, LocalIconReference multiplicationID)
    ReplaceParameter(multiplicationID, 0, BaseIconParameter 0)
    ReplaceParameter(recurseFactorialID, 0, LocalIconReference subtractionID)
    ReplaceParameter(multiplicationID, 1, LocalIconReference recurseFactorialID) ]

let localIcons = Map.empty |> applyActions actions
let factorialCustomIcon = {MainIconID = ifID; ParameterCount = 1; LocalIcons = localIcons}
let customIcons = Map.empty |> Map.add "factorial" factorialCustomIcon

let context =
  { CustomIcons = customIcons
    EvaluatedParams = [0]
    LocalIconInstances = localIcons
    CurrentIconID = ifID }

let iconFactorial n =
  let context = {context with EvaluatedParams = [n]}
  let instruction = localIcons[ifID]
  eval context instruction