module AdventOfCode.TestsDay8

open NUnit.Framework
open AdventOfCode
open System.IO
open FsUnit

let sampleInput = "RL

AAA = (BBB, CCC)
BBB = (DDD, EEE)
CCC = (ZZZ, GGG)
DDD = (DDD, DDD)
EEE = (EEE, EEE)
GGG = (GGG, GGG)
ZZZ = (ZZZ, ZZZ)" |> Common.SplitInput

let sampleInput2 = "LLR

AAA = (BBB, BBB)
BBB = (AAA, ZZZ)
ZZZ = (ZZZ, ZZZ)" |> Common.SplitInput

let realInput = "day8.input" |> File.ReadAllText |> Common.SplitInput

[<Test>]
let parseInput() =
    let (instructions, network) = Day8.parseInput sampleInput
    instructions |> should equal (seq { Day8.Right; Day8.Left })
    network.Count |> should equal 7

[<Test>]
let walk() = 
    Day8.walk (Day8.parseInput sampleInput) |> should equal 2
    Day8.walk (Day8.parseInput sampleInput2) |> should equal 6
    Day8.walk (Day8.parseInput realInput) |> should equal 16531