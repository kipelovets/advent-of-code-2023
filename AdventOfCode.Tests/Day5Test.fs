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
        let expected: int64 list = [79; 14; 55; 13]
        almanac.seeds |> should equal expected
        almanac.maps.Length |> should equal 7
        almanac.maps[0] 79 |> should equal 81

    [<Test>]
    member _.processAlmanac() =
        Day5.processAlmanac (Day5.parseInput sampleInput) |> should equal 35
        Day5.processAlmanac (Day5.parseInput realInput) |> should equal 650599855