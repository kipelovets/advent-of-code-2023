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

let sampleInputPart2 = """LR

11A = (11B, XXX)
11B = (XXX, 11Z)
11Z = (11B, XXX)
22A = (22B, XXX)
22B = (22C, 22C)
22C = (22Z, 22Z)
22Z = (22B, 22B)
XXX = (XXX, XXX)""" |> Common.SplitInput

let realInput = "day8.input" |> File.ReadAllText |> Common.SplitInput

[<Test>]
let parseInput() =
    let (instructions, network) = Day8.parseInput sampleInput
    instructions |> should equal (seq { Day8.Right; Day8.Left })
    network.Count |> should equal 7

[<Test>]
let walk() = 
    Day8.walk (Day8.parseInput sampleInput) "AAA" |> should equal 2
    Day8.walk (Day8.parseInput sampleInput2) "AAA" |> should equal 6
    Day8.walk (Day8.parseInput realInput) "AAA" |> should equal 16531

[<Test>]
let walkAll() =
    Day8.walkAll (Day8.parseInput sampleInputPart2) |> should equal 6
    Day8.walkAll (Day8.parseInput realInput) |> should equal 24035773251517L