module AdventOfCode.TestsDay6

open NUnit.Framework
open AdventOfCode
open System.IO
open FsUnit

[<TestFixture>]
type Day6Test () =
    let sampleInput = "Time:      7  15   30
Distance:  9  40  200" |> Common.SplitInput |> Array.toList

    let realInput = "day6.input" |> File.ReadAllText |> Common.SplitInput |> Array.toList

    [<Test>]
    member _.parseInput() =
        let result = Day6.parseInput sampleInput
        result |> should equal [(7, 9); (15, 40); (30, 200)]

    [<Test>]
    member _.winOptions() =
        Day6.winOptions (7, 9) |> should equal [2; 3; 4; 5]
        Day6.winOptions (15, 40) |> should haveLength 8
        Day6.winOptions (30, 200) |> should haveLength 9
        
    [<Test>]
    member _.errorMargin() =
        Day6.errorMargin (Day6.parseInput sampleInput) |> should equal 288
        Day6.errorMargin (Day6.parseInput realInput) |> should equal 4568778