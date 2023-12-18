module AdventOfCode.Common

open System.Text.RegularExpressions

exception AdventError of string

let SplitInput (input: string) : string list =
    input.Split '\n' 
        |> Array.map (fun e -> e.Trim()) 
        |> Array.filter (fun e -> e.Length > 0)
        |> Array.toList 

let (|Regex|_|) pattern input =
    let m = Regex.Match(input, pattern)
    if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
    else None