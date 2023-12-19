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
    | Common.Regex @"(\w+) = \((\w+), (\w+)\)" [from; left; right] ->
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

let nextStep (input: Input) (stepsMade: int) (currentLoc: Location): Location =
    let (instructions, network) = input
    let side = instructions[stepsMade % instructions.Length]
    let (left, right) = network[currentLoc]
    match side with
    | Left -> left
    | Right -> right

let nextTurn (input: Input) (stepsMade: int): string =
    let (instructions, _) = input
    let side = instructions[stepsMade % instructions.Length]
    match side with
    | Left -> "left"
    | Right -> "right"

let walk (input: Input) (l: Location): int =
    let rec walkRec (loc: Location) (stepsMade: int): int =
        match loc with
        | "ZZZ" -> stepsMade
        | _ -> walkRec (nextStep input stepsMade loc) (stepsMade + 1)
    
    walkRec l 0

let rec gcd a b = if b = 0L then a else gcd b (a % b)

let lcm a b =
    let res = (a * b) / gcd a b
    res

let walk2 (input: Input) (l: Location): int =
    let rec walkRec (loc: Location) (stepsMade: int): int =
        if loc[2] = 'Z' then
            stepsMade
        else 
            walkRec (nextStep input stepsMade loc) (stepsMade + 1)
    
    walkRec l 0

let walkAll ((instructions, network): Input): int64 =
    let initialNodes = network.Keys |> Seq.filter (fun loc -> loc[2] = 'A')
    let steps = initialNodes |> Seq.map (walk2 (instructions, network))
    steps |> Seq.map int64 |> Seq.fold lcm 1L
    