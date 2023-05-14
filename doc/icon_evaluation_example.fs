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

let rec eval iconContext instruction =
    match instruction with
    | Primitive n -> n
    | Trap -> failwith "Trap sprung"
    | Unary(operator, operand) -> 0
    | Binary(operator, leftOperand, rightOperand) -> 0
    | If(cond, trueBranch, falseBranch) ->
        let res = eval iconContext cond
        if res = 0 then eval iconContext trueBranch
        else eval iconContext falseBranch
    | Icon(name, parameters) ->
        let evaluatedParameters = List.map (eval iconContext) parameters
        let newContext = {iconContext with Parameters = evaluatedParameters}
        let nextInstruction = fetchIconInstruction name
        eval newContext nextInstruction
    | Parameter index -> iconContext.Parameters[index]
