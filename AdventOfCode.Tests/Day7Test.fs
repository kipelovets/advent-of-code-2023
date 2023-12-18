module AdventOfCode.TestsDay7

open NUnit.Framework
open AdventOfCode
open System.IO
open FsUnit

let sampleInput = 
    "32T3K 765
T55J5 684
KK677 28
KTJJT 220
QQQJA 483" |> Common.SplitInput

let realInput = "day7.input" |> File.ReadAllText |> Common.SplitInput

[<Test>]
let parseInput() =
    let result = Day7.parseInput sampleInput
    result |> should haveLength 5
    result[0] |> should equal ("32T3K", 765)

[<TestCase("AAAAA", 0)>]
[<TestCase("AAAAB", 1)>]
[<TestCase("AAABB", 2)>]
[<TestCase("AAABC", 3)>]
[<TestCase("AABBC", 4)>]
[<TestCase("AABCD", 5)>]
[<TestCase("ABCDE", 6)>]
let handType (hand: Day7.Hand, expectedType: int) =
    Day7.handType hand |> should equal expectedType

[<TestCase("AAAAA", "AAAAA", 0)>]
[<TestCase("AAAAA", "KKKKK", -1)>]
[<TestCase("AAAAA", "AAAAK", -1)>]
[<TestCase("99977", "99988", 1)>]
let compareHands  (hand1: Day7.Hand, hand2: Day7.Hand, expected: int) = 
    Day7.compareHands hand1 hand2 |> should equal expected

[<Test>]
let sortHands() =
    Day7.sortHands (Day7.parseInput sampleInput |> Seq.map fst) 
        |> should equal [ "32T3K"; "KTJJT"; "KK677"; "T55J5"; "QQQJA"; ]

[<Test>]
let part1() =
    Day7.part1 (Day7.parseInput sampleInput) |> should equal 6440
    // Day7.part1 (Day7.parseInput realInput) |> should equal 253638586

[<Test>]
let part2() =
    Day7.part2 (Day7.parseInput sampleInput) |> should equal 5905
    // Day7.part2 (Day7.parseInput realInput) |> should equal 253253225