module AdventOfCode.TestsDay5

open NUnit.Framework
open AdventOfCode
open System.IO
open FsUnit

let SplitInput (input: string) : string array =
        input.Split '\n' |> Array.map (fun e -> e.Trim()) 

[<TestFixture>]
type Day5Test () =

    let sampleInput = 
        "seeds: 79 14 55 13

seed-to-soil map:
50 98 2
52 50 48

soil-to-fertilizer map:
0 15 37
37 52 2
39 0 15

fertilizer-to-water map:
49 53 8
0 11 42
42 0 7
57 7 4

water-to-light map:
88 18 7
18 25 70

light-to-temperature map:
45 77 23
81 45 19
68 64 13

temperature-to-humidity map:
0 69 1
1 0 69

humidity-to-location map:
60 56 37
56 93 4" 
        |> SplitInput |> Array.toList

    let realInput = "day5.input" 
                    |> File.ReadAllText 
                    |> SplitInput |> Array.toList

    [<Test>]
    member _.parseInput() =
        let almanac = Day5.parseInput sampleInput
        let expectedSeeds: int64 list = [79; 14; 55; 13]
        almanac.seeds |> should equal expectedSeeds
        almanac.maps.Length |> should equal 7
        almanac.maps[0] [(79, 1)] |> should equal [(int64 81, int64 1)]

    [<Test>]
    member _.processAlmanac() =
        Day5.processAlmanac (Day5.parseInput sampleInput) |> should equal 35
        Day5.processAlmanac (Day5.parseInput realInput) |> should equal 650599855

    [<Test>]
    member _.processAlmanacPart2() = 
        let sampleAlmanac = (Day5.parseInput sampleInput)
        let testAlmanac: Day5.Almanac = {seeds=[int64 82; int64 1]; maps=sampleAlmanac.maps}
        Day5.processAlmanacRanges testAlmanac |> should equal 46
        Day5.processAlmanacRanges sampleAlmanac |> should equal 46
        Day5.processAlmanacRanges (Day5.parseInput realInput) |> should equal 1240035