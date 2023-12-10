namespace AdventOfCode

open System.Text.RegularExpressions


module Day5 =
    type Seed = int64
    type Map = Seed->Seed
    type Block =
        | Seeds of Seed array
        | Mapping of Map
    type MapRecord = Seed*Seed*Seed
    exception AdventError of string
    type Almanac = {seeds: Seed list; maps: Map list}

    let makeMap (recs: MapRecord list) : Map =
        let res (input: int64): int64 =
            let matchingRecs = recs |> List.filter (fun (_, src, length) -> input >= src && input < src + length)
            match matchingRecs with
            | [] -> input
            | [(dst, src, _)] -> input - src + dst
            | _ -> raise (AdventError "Oopsie")
        res

    let makeMapRecord (s: int64 array) : MapRecord =
        match (s |> Array.toList) with
        | [a; b; c] -> (a, b, c)
        | _ -> raise (AdventError "Fail")

    let parseInput (lines: string list): Almanac = 
        let parseBlock (lines: string list) : Block = 
            match lines with
            | [] -> Mapping (makeMap []) // raise (AdventError "Empty block")
            | [first] -> 
                Regex.Replace(first, "seeds: ", "").Split(" ") |> Array.map int64 |> Seeds
            | _::rest -> 
                rest |> List.map (fun s -> s.Split " " |> Array.map int64 |> makeMapRecord ) |> makeMap |> Mapping


        let rec parse (rest: string list) (curr: string list) : Block list =
            match rest with
            | ""::rst -> (parseBlock curr)::(parse rst List.empty)
            | line::rst -> (parse rst (curr @ [line]))
            | [] -> [parseBlock curr]

        let res = parse lines List.empty

        let seeds = res |> List.find (fun x -> match x with
                                                 | Seeds(_) -> true
                                                 | _ -> false) |> (fun x -> match x with
                                                                                     | Seeds(x) -> x
                                                                                     | _ -> raise (AdventError "")) |> Array.toList
        let maps = res |> List.filter (fun x -> match x with
                                                | Mapping(_) -> true
                                                | _ -> false) |> List.map  (fun x -> match x with
                                                                                     | Mapping(x) -> x
                                                                                     | _ -> raise (AdventError ""))

        {seeds=seeds; maps=maps}

    let rec applyMaps (maps: Map list) (seed: Seed) : Seed =
        match maps with
        | first::rest -> (applyMaps rest (first seed))
        | [] -> seed

    let processAlmanac (almanac: Almanac): Seed =
        let applyTheseMaps = applyMaps almanac.maps
        almanac.seeds |> List.map applyTheseMaps |> List.min