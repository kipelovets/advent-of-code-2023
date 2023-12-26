module AdventOfCode.Day10

type Direction =
    | North
    | South
    | West
    | East

type Pipe = Direction * Direction

type Node =
    | Pipe of Pipe
    | Ground
    | Start

type Landscape = Node array array

let opposite (dir: Direction): Direction =
    match dir with
    | North -> South
    | South -> North
    | West -> East
    | East -> West

let connects (nodeFrom: Node) (nodeTo: Node) (dir: Direction): bool =
    match (nodeFrom, nodeTo) with
    | (Ground, _) | (_, Ground) -> false
    | (Start, Pipe(x, y)) -> 
        opposite x = dir || opposite y = dir
    | (Pipe(x, y), Start) -> x = dir || y = dir
    | (Pipe (x1, y1), Pipe (x2, y2)) when x1 = dir || y1 = dir -> 
        let opp = opposite dir
        opp = x2 || opp = y2
    | _ -> false

let parseInput (input: string seq): Landscape = 
    let convertChar (c: char): Node =  
        match c with
        | '|' -> Pipe(North, South)
        | '-' -> Pipe(East, West)
        | 'L' -> Pipe(North, East)
        | 'J' -> Pipe(North, West)
        | '7' -> Pipe(South, West)
        | 'F' -> Pipe(South, East)
        | '.' -> Ground
        | 'S' -> Start

    let convertLine (line: string): Node array =
        line |> Seq.map convertChar |> Seq.toArray

    input |> Seq.map convertLine |> Seq.toArray

type Coords = int * int

let move ((x, y): Coords) (dir: Direction): Coords =
    match dir with
    | North -> (x, y - 1)
    | South -> (x, y + 1)
    | West -> (x - 1, y)
    | East -> (x + 1, y)

let connectedNodes (island: Landscape) (cur: Coords): (Direction * Coords) seq =
    let len = island.Length
    let (x, y): Coords = cur
    let nextDirs = seq {
        if x > 0 then yield West
        if x < len - 1 then yield East
        if y > 0 then yield North
        if y < len - 1 then yield South }

    let nextNodes: (Direction * Coords) seq = 
        nextDirs 
        |> Seq.map (fun dir -> (dir, move cur dir))
        |> Seq.filter (fun (dir, (x2, y2)) -> connects (island[y][x]) (island[y2][x2]) dir)

    nextNodes

let rec findLoop (island: Landscape) (start: Coords) (path: Coords seq): int option =
    let cur = path |> Seq.last
    let nextNodes = connectedNodes island cur |> Seq.map (fun (_, c) -> c)

    let pathLen = path |> Seq.length
    if Seq.contains start nextNodes && pathLen > 2 then
        Some(pathLen / 2 + pathLen % 2)
    else
        let loopLengths = nextNodes |> Seq.filter (fun n -> not (Seq.contains n path))
                                    |> Seq.map (fun n -> findLoop island start (Seq.append path [n]))
                                    |> Seq.filter Option.isSome
                                    |> Seq.map Option.get

        if Seq.length loopLengths > 0 then
            Some(Seq.head loopLengths)
        else 
            None

[<TailCall>]
let rec findLoop2 (island: Landscape) (prev: Coords) (dir: Direction) (steps: int): int option =
    let (x, y) = move prev dir
    let backDir = opposite dir
    
    match island[y][x] with
    | Start -> Some(steps + 1)
    | Ground -> None
    | Pipe(f, t) -> 
        let nextDir = [f; t] |> Seq.filter ((<>) backDir) |> Seq.head
        findLoop2 island (x, y) nextDir (steps + 1)
       
let findStart (island: Landscape): Coords = 
    let len = island.Length
    seq { 
        for x in [0..len-1] do
            for y in [0..len-1] do
                if island[y][x] = Start then yield (x, y) } |> Seq.head

let solve (island: Landscape): int =
    let start: Coords = findStart island
    let afterStart = connectedNodes island start |> Seq.map (fun (d, _) -> d)

    let res = afterStart 
            |> Seq.map (fun dir -> findLoop2 island start dir 0)
            |> Seq.filter Option.isSome
            |> Seq.map Option.get
            |> Seq.head
    
    res / 2
