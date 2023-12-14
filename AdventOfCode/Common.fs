namespace AdventOfCode

module Common =
    exception AdventError of string

    let SplitInput (input: string) : string list =
        input.Split '\n' 
            |> Array.map (fun e -> e.Trim()) 
            |> Array.filter (fun e -> e.Length > 0)
            |> Array.toList 
