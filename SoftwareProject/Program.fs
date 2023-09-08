module System.Console

// Data representation
type Instruction =
    | Trap
    | Primitive of int
    | Unary of char * Instruction
    | Binary of char * Instruction * Instruction
    | If of Instruction * Instruction * Instruction
    | Icon of string * Instruction list
    | Parameter of int

type Context =
    { Parameters: int list }

let fetchIconInstruction name =
    Trap

let FalseValue = 0
let TrueValue = 1

let rec eval iconContext instruction =
    let boundEval = (eval iconContext)
    match instruction with
    | Primitive n -> n
    | Trap -> failwith "Trap sprung"
    | Unary(operator, operand) -> 0
    | Binary(operator, leftOperand, rightOperand) -> 0
    | If(cond, trueBranch, falseBranch) ->
        let res = boundEval cond
        if res = FalseValue then boundEval falseBranch
        else boundEval trueBranch
    | Icon(name, parameters) ->
        let evaluatedParameters = List.map boundEval parameters
        let newContext = {iconContext with Parameters = evaluatedParameters}
        let nextInstruction = fetchIconInstruction name
        eval newContext nextInstruction
    | Parameter index -> iconContext.Parameters[index]


type CustomIcon =
    { InstructionTree : Instruction
      ParameterCount : int
      IconContext : DrawnIcon list }

and DrawnIcon =
    { Name : string
      xPosition : int
      yPosition : int
      Result : int Option
      Parameters : int Option list}


let runCommand input =
    failwith "Not implemented"

let rec readConsole stop =
    let input = Console.ReadLine()
    runCommand input
    match stop with
    | true -> ()
    | false -> readConsole stop