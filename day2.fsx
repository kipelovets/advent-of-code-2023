open System.Text.RegularExpressions
open System.IO
open System

exception E of string

let sampleInput = 
    "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green".Split '\n' |> Array.map (fun e -> e.Trim())

type Set = {Red: int; Green: int; Blue: int}
type Game = {Id: int; Sets: Set array}

let defaultSet = {Red = 0; Green = 0; Blue = 0}

let parseDraw (s: Set) (str: string) : Set =
    let d = str.Split ' '
    let count = int d[0]
    let color = d[1]
    match color with
        | "red" -> {s with Red = count}
        | "green" -> {s with Green = count}
        | "blue" -> {s with Blue = count}
        | _ -> raise (E(sprintf "Unknown color: %s" color))

let parseSet (str: string): Set = 
    let draws = str.Split ", " 
    Array.fold parseDraw defaultSet draws

let parseGame (line: string) : Game =
    let regex = Regex(@"Game (\d+): (.+)")
    let m = regex.Match(line)
    let id = int m.Groups[1].Value
    let sets = 
        m.Groups[2].Value.Split "; "
        |> Array.map parseSet
    
    {Id = id; Sets = sets}

let maxSet: Set = {Red = 12; Green = 13; Blue = 14}
let isGamePossible (game: Game): bool =
    game.Sets 
        |> Array.filter (fun (s: Set) -> s.Red <= maxSet.Red && s.Green <= maxSet.Green && s.Blue <= maxSet.Blue)
        |> (fun sets -> sets.Length = game.Sets.Length)

let possibleGames (input: string array) : int =
    input 
            |> Array.map parseGame
            |> Array.filter isGamePossible
            |> Array.map (fun game -> game.Id)
            |> Array.sum

printfn "Day2.1 sample: %d" (possibleGames sampleInput)

let realInput = 
    "day2.input" 
        |> File.ReadAllText 
        |> (fun e -> e.Split '\n') 
        |> Array.map (fun e -> e.Trim())
        |> Array.filter (fun e -> e.Length > 0)

printfn "Day2.1: %d" (possibleGames realInput)

let sampleInput2 =
    "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green".Split '\n' |> Array.map (fun e -> e.Trim())

let minPossibleBag (game: Game) : Set =
    {Red = game.Sets |> Array.map (fun e -> e.Red) |> Array.max;
    Green = game.Sets |> Array.map (fun e -> e.Green) |> Array.max;
    Blue = game.Sets |> Array.map (fun e -> e.Blue) |> Array.max;}

let sumPower (input: string array) : int =
    input 
        |> Array.map parseGame
        |> Array.map minPossibleBag
        |> Array.map (fun e -> e.Red * e.Green * e.Blue)
        |> Array.sum

printfn "Day2.2 sample: %d" (sumPower sampleInput2)
printfn "Day2.2: %d" (sumPower realInput)
