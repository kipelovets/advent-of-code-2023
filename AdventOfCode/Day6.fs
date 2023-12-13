namespace AdventOfCode

module Day6 =
    type Race = int64*int64

    let After (search: string) (str: string) = 
        str[str.IndexOf(search) + 1..]

    let Split (by: string) (str: string) =
        str.Split(by) 

    let parseInputLine (line: string): int64 array =
        line 
            |> (After ":") 
            |> (Split " ") 
            |> Array.filter (fun s -> s.Length > 0)
            |> Array.map int64

    let parseInput (input: string list): Race list = 
        let [times; distances] = input |> List.map parseInputLine
        [ for i in [0..times.Length - 1] do (times[i], distances[i]) ]

    let winOptions ((time, dist): Race): int64 list =
        let min = 1L
        let max = time - 1L
        [for i in min..max do 
            if (time - i) * i > dist then i]

    let errorMargin (races: Race list): int =
        races 
            |> List.map (fun race -> (winOptions race).Length) 
            |> List.fold (*) 1

    let parseInput2 (input: string list): Race =
        let [time; dist] = input 
                            |> List.map (fun str -> (After ":" str).Replace(" ", "") |> int64 )
        (time, dist)
            