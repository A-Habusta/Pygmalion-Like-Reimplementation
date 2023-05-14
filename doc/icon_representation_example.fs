type CustomIcon =
    { InstructionTree : Instruction
      ParameterCount : int
      IconContext : DrawnIcon list }

and DrawnIcon =
    { Name : string
      xPosition : int
      yPosition : int
      Result : int Option
      Parameters : int Option list }
