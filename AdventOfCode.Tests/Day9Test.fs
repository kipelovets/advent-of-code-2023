module AdventOfCode.TestsDay9

open NUnit.Framework
open AdventOfCode
open System.IO
open FsUnit

let sampleInput = "0 3 6 9 12 15
1 3 6 10 15 21
10 13 16 21 30 45" |> Common.SplitInput

let realInput = "day9.input" |> File.ReadAllText |> Common.SplitInput

[<Test>]
let parseInput() =
    sampleInput 
    |> Day9.parseInput 
    |> Seq.head
    |> should equal [0; 3; 6; 9; 12; 15]

[<Test>]
let differences() =
    let first = 
        sampleInput 
        |> Day9.parseInput 
        |> Seq.head 
        |> Day9.differences 
    first |> should equal [3; 3; 3; 3; 3]
    first |> Day9.differences |> should equal [0; 0; 0; 0]

[<Test>]
let predictNext() =
    sampleInput
    |> Day9.parseInput
    |> Seq.head
    |> Day9.predictNext
    |> should equal 18

[<Test>]
let predictAll() =
    sampleInput
    |> Day9.parseInput
    |> Day9.predictNextAll
    |> should equal 114

    realInput
    |> Day9.parseInput
    |> Day9.predictNextAll
    |> should equal 1641934234