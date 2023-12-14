module AdventOfCode.TestsDay4

open NUnit.Framework
open AdventOfCode
open System.IO
open FsUnit

[<TestFixture>]
type Day4Test () =

    let realInput = "day4.input" 
                    |> File.ReadAllText 
                    |> Common.SplitInput
                    |> List.toArray
                    |> Array.map Day4.parseInputLine

    let expectedFirstSampleLine = ([| 41; 48; 83; 86; 17 |], [| 83; 86;  6; 31; 17;  9; 48; 53 |])

    [<Test>]
    member _.parseInputLine() =
        Day4.parseInputLine "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53"
            |> should equal expectedFirstSampleLine

    [<Test>]
    member _.sampleInput() =
        Day4.sampleInput[0] |> should equal expectedFirstSampleLine

    [<TestCase(0, 8)>]
    [<TestCase(1, 2)>]
    [<TestCase(2, 2)>]
    [<TestCase(3, 1)>]
    [<TestCase(4, 0)>]
    [<TestCase(5, 0)>]
    member _.cardValue(cardInd: int, expected: int) =
        Day4.cardValue Day4.sampleInput[cardInd] |> should equal expected

    [<Test>]
    member _.cardPileValueSample() =
        Day4.cardPileValue Day4.sampleInput |> should equal 13


    [<Test>]
    member _.cardPileValueReal() =
        Day4.cardPileValue realInput |> should equal 15268

    [<Test>]
    member _.totalWonCards() =
        Day4.totalWonCards (Day4.sampleInput |> Array.toList) |> should equal 30
        Day4.totalWonCards (realInput |> Array.toList) |> should equal 6283755

    