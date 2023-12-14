module AdventOfCode.TestsDay6

open NUnit.Framework
open AdventOfCode
open System.IO
open FsUnit

[<TestFixture>]
type Day6Test () =
    let sampleInput = "Time:      7  15   30
Distance:  9  40  200" |> Common.SplitInput

    let realInput = "day6.input" |> File.ReadAllText |> Common.SplitInput

    [<Test>]
    member _.parseInput() =
        let result = Day6.parseInput sampleInput
        result |> should equal [(7L, 9L); (15L, 40L); (30L, 200L)]

    [<Test>]
    member _.winOptions() =
        Day6.winOptions (7, 9) |> should equal [2L; 3L; 4L; 5L]
        Day6.winOptions (15, 40) |> should haveLength 8
        Day6.winOptions (30, 200) |> should haveLength 9
        
    [<Test>]
    member _.errorMargin() =
        Day6.errorMargin (Day6.parseInput sampleInput) |> should equal 288
        Day6.errorMargin (Day6.parseInput realInput) |> should equal 4568778

    [<Test>]
    member _.parseInput2() =
        Day6.parseInput2 sampleInput |> should equal (71530L, 940200L)
        Day6.parseInput2 realInput |> should equal (48989083L, 390110311121360L)

    [<Test>]
    member _.part2() =
        Day6.errorMargin [Day6.parseInput2 realInput] |> should equal 28973936