open System.Text.RegularExpressions
open System.IO
open System

let sampleInput = 
    "1abc2
pqr3stu8vwx
a1b2c3d4e5f
treb7uchet"

let digits : string list = 
    [ "zero"; "one"; "two"; "three"; "four"; "five"; "six"; "seven"; "eight"; "nine" ]

let convertLine (line: string) =
    Regex.Replace(line, "[a-z]", "")
        |> (fun numbers -> numbers.Trim())
        |> (fun numbers -> int (numbers[0].ToString() + numbers[numbers.Length - 1].ToString()))

let calibrationValue (lines : string array) =
    lines |> Array.toList 
        |> List.filter (fun e -> e.Length > 0)
        |> List.map convertLine
        |> List.sum

printfn "Day1.1 sample: %d" (calibrationValue (sampleInput.Split '\n'))

let realInputLines = 
    "day1.input" 
        |> File.ReadAllText 
        |> (fun e -> e.Split '\n') 
        |> Array.map (fun e -> e.Trim())
        |> Array.filter (fun e -> e.Length > 0)

printfn "Day1.1: %d" ( realInputLines |> calibrationValue )

let sampleInput2 = 
    "two1nine
eightwothree
abcone2threexyz
xtwone3four
4nineeightseven2
zoneight234
7pqrstsixteen"

let rec replaceDigits (str: string) : string =
    let indexes = 
        digits 
            |> List.map (fun (dig: string) -> (str.IndexOf(dig), dig))
            |> List.filter (fun (index: int, _: string) -> -1 <> index)
    if indexes.Length = 0 then
        str
    else
        let _, digit = indexes |> List.minBy (fun (index: int, _: string) -> index)
        let replacement = string digit[0] + string (Array.IndexOf(List.toArray(digits), digit)) + digit[1..]
        let res = str.Replace(digit, replacement)

        let lastIndexes = 
            digits 
                |> List.map (fun d -> (str.LastIndexOf(d), d))
                |> List.filter (fun (index: int, _: string) -> -1 <> index)

        if lastIndexes.Length = 0 then
            res
        else
            let _, digit = lastIndexes |> List.maxBy (fun (index: int, _: string) -> index)
            let replacement = string digit[0] + string (Array.IndexOf(List.toArray(digits), digit)) + digit[1..]
            res.Replace(digit, replacement)

printfn "Day1.2 sample: %d" (sampleInput2.Split '\n' 
            |> Array.map (fun e -> e.Trim())
            |> Array.map replaceDigits
            |> calibrationValue)

let inputWithReplacedDigits = realInputLines |> Array.map replaceDigits
printfn "Day1.2: %d" (inputWithReplacedDigits |> calibrationValue)
