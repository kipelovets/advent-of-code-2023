module AdventOfCode.TestsDay10

open NUnit.Framework
open AdventOfCode
open System.IO
open FsUnit
open Day10

let sampleInput1 = Common.SplitInput "
7-F7-
.FJ|7
SJLL7
|F--J
LJ.LJ" 

let sampleInput2 = Common.SplitInput "
-L|F7
7S-7|
L|7||
-L-J|
L|-JF"


let realInput = "day10.input" |> File.ReadAllText |> Common.SplitInput

[<Test>]
let testParseInput() =
    let result = parseInput sampleInput1
    result.Length |> should equal 5
    result[0][0] |> should equal (Pipe(South, West))

[<Test>]
let testConnects() =
    connects (Pipe(South, West)) Ground West |> should equal false
    connects (Pipe(South, West)) Start West |> should equal true
    connects (Pipe(South, West)) Start East |> should equal false
    connects (Pipe(South, West)) (Pipe(North, South)) West |> should equal false
    connects (Pipe(South, West)) (Pipe(North, South)) South |> should equal true
    let island = parseInput sampleInput1
    connects (island[2][0]) (island[2][1]) East |> should equal true

[<Test>]
let testConnectedNodes() =
    let island = parseInput sampleInput1
    connectedNodes island (0, 2) |> should equal [(1, 2); (0, 3)]
    connectedNodes island (1, 2) |> should equal [(0, 2); (1, 1)]

[<Test>]
let testSolve() =
    sampleInput1 |> parseInput |> solve |> should equal 8
    sampleInput2 |> parseInput |> solve |> should equal 4
    realInput |> parseInput |> solve |> should equal 4
