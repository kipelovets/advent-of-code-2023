namespace AdventOfCode

module Common =
    let SplitInput (input: string) : string array =
        input.Split '\n' 
            |> Array.map (fun e -> e.Trim()) 
            |> Array.filter (fun e -> e.Length > 0)
