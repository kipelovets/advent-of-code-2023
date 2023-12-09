module AdventOfCode.Tests

open NUnit.Framework
open AdventOfCode
open System.IO

[<TestFixture>]
type Day3Test () =

    let realInput = "day3.input" |> File.ReadAllText |> Common.SplitInput

    [<Test>]
    member this.findPartNumbersRow() =
        let result = Day3.findPartNumbersRow Day3.sampleInput 0
        Assert.That(result, Is.EqualTo([(0, 2)]))

    [<Test>]
    member this.Day3Part1() = 
        let result = Day3.partNumberSum Day3.sampleInput
        Assert.That(result, Is.EqualTo(4361))

    [<Test>]
    member this.Day3() = 
        let result = Day3.partNumberSum realInput
        Assert.That(result, Is.EqualTo(527364))

    [<TestCase(-1, -1)>]
    [<TestCase(0, 0)>]
    [<TestCase(2, 0)>]
    [<TestCase(3, -1)>]
    member this.findNumberStart(s: int, expected: int) = 
        let res = match Day3.findNumberStart Day3.sampleInput s 0 with
                    | Some(x) -> x
                    | None -> -1
        Assert.That(
            res,
            Is.EqualTo(expected))

    [<TestCase(0, 0, 467)>]
    [<TestCase(1, 0, 67)>]
    [<TestCase(2, 0, 7)>]
    [<TestCase(3, 0, -1)>]
    [<TestCase(5, 0, 114)>]
    [<TestCase(2, 2, 35)>]
    member this.extractNumber(x: int, y: int, expected: int) = 
        let res = match Day3.extractNumber Day3.sampleInput x y with
                    | Some(x) -> x
                    | None -> -1
        Assert.That(
            res,
            Is.EqualTo(expected))

    [<Test>]
    member this.gearTouchingNumbersRealInput() =
        let res = Day3.gearTouchingNumbers realInput 48 2
        Assert.That(res, Is.EqualTo([906; 358]))

    [<TestCase(3, 1, 16345)>]
    [<TestCase(5, 8, 451490)>]
    [<TestCase(0, 0, 0)>]
    member this.gearRatioSampleInput(x: int, y: int, expected: int) =
        let res = Day3.gearRatio Day3.sampleInput x y
        Assert.That(res, Is.EqualTo(expected))

    [<TestCase(48, 2, 324348)>]
    member this.gearRatioRealInput(x, y, expected) =
        let res = Day3.gearRatio realInput x y
        Assert.That(res, Is.EqualTo(expected))

    [<Test>]
    member this.gearRatios() = 
        Assert.That(Day3.gearRatios Day3.sampleInput, Is.EqualTo(467835))

    [<Test>]
    member this.Day3Part2() = 
        // 63810571 too low
        Assert.That(Day3.gearRatios realInput, Is.EqualTo(79026871))
