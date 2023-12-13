namespace AdventOfCode

module Day6 =
    type Race = int*int

    let After (search: string) (str: string) = 
        str[str.IndexOf(search) + 1..]

    let Split (by: string) (str: string) =
        str.Split(by) 

    let parseInputLine (line: string): int array =
        line 
            |> (After ":") 
            |> (Split " ") 
            |> Array.filter (fun s -> s.Length > 0)
            |> Array.map int

    let parseInput (input: string list): Race list = 
        let [times; distances] = input |> List.map parseInputLine
        [ for i in [0..times.Length - 1] do (times[i], distances[i]) ]

    let winOptions ((time, dist): Race): int list =
        let min = 1
        let max = time - 1
        [for i in min..max do 
            if (time - i) * i > dist then i]

    let errorMargin (races: Race list): int =
        races 
            |> List.map (fun race -> (winOptions race).Length) 
            |> List.fold (*) 1