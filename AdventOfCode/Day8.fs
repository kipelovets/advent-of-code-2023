module AdventOfCode.Day8

type Location = string
type Node = Location*Location
type Network = Map<Location, Node>
type Instruction = 
    | Left
    | Right
type Input = Instruction array * Network

let parseInstruction (c: char): Instruction =
    match c with
    | 'R' -> Right
    | 'L' -> Left
    | _ -> raise (Common.AdventError $"Unknown symbol {c}")

let parseLine (line: string): (Location*Node) =
    match line with
    | Common.Regex @"([A-Z]+) = \(([A-Z]+), ([A-Z]+)\)" [from; left; right] ->
        (from, (left, right))
    | _ -> raise (Common.AdventError $"Can't parse line {line}")


let parseInput (input: string seq): Input = 
    let instructions = 
        Seq.head input 
        |> Seq.map parseInstruction
        |> Seq.toArray
    let network = 
        Seq.tail input 
        |> Seq.map parseLine
        |> Map.ofSeq
    
    (instructions, network)

let nextStep (input: Input, stepsMade: int, currentLoc: Location): Location =
    let (instructions, network) = input
    let side = instructions[stepsMade % instructions.Length]
    let (left, right) = network[currentLoc]
    match side with
    | Left -> left
    | Right -> right

let walk (input: Input): int =
    let rec walkRec (loc: Location) (stepsMade: int): int =
        match loc with
        | "ZZZ" -> stepsMade
        | _ -> walkRec (nextStep (input, stepsMade, loc)) (stepsMade + 1)
    
    walkRec "AAA" 0