open System.IO

let args = fsi.CommandLineArgs
if args.Length = 1 then
    printfn $"Usage: {args[0]} <day number>"
    exit 1
let num = int args[1]

let codeTemplate = $"namespace AdventOfCode

module Day{num} =
    let parseInput input = 
        0
"

let testTemplate = $"""module AdventOfCode.TestsDay{num}

open NUnit.Framework
open AdventOfCode
open System.IO
open FsUnit

[<TestFixture>]
type Day{num}Test () =
    let sampleInput = ""

    let realInput = "day{num}.input" |> File.ReadAllText 

    [<Test>]
    member _.parseInput() =
        let result = Day{num}.parseInput sampleInput
        result |> should equal 0

"""

let codeProj = "AdventOfCode"
let testProj = "AdventOfCode.Tests"
let codeFile = $"Day{num}.fs"
let testFile = $"Day{num}Test.fs"
let inputFile = $"day{num}.input"


File.WriteAllText ($"{codeProj}/{codeFile}", codeTemplate)
File.WriteAllText ($"{testProj}/{inputFile}", "")
File.WriteAllText ($"{testProj}/{testFile}", testTemplate)