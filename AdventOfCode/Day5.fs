namespace AdventOfCode

open System.Text.RegularExpressions


module Day5 =
    open System.Collections.Generic
    type Seed = int64
    type SeedRange = Seed*Seed
    type Map = SeedRange list->SeedRange list
    type Block =
        | Seeds of Seed array
        | Mapping of Map
    type MapRecord = Seed*Seed*Seed
    exception AdventError of string
    type Almanac = {seeds: Seed list; maps: Map list}

    let makeMap (recs: MapRecord list) : Map =
        let rec singleSeedMap ((inputFrom, inputLength): SeedRange): SeedRange list =
            let inputTo = inputFrom + inputLength
            try 
                let (dst, src, length) = 
                    recs 
                    |> List.find (fun (_, src, length) -> 
                            (inputFrom >= src && inputFrom < src + length) ||
                                (inputTo > src && inputTo < src + length))
                let srcTo = src + length
                let newFrom = max src inputFrom
                let currentResult: SeedRange = (newFrom - src + dst, (min inputTo srcTo) - newFrom) 

                currentResult 
                    :: (if inputFrom < src then (singleSeedMap (inputFrom, src - inputFrom)) else [])
                    @ (if inputTo > srcTo then (singleSeedMap (srcTo, inputTo - srcTo)) else [])
            with
            | :? KeyNotFoundException -> [(inputFrom, inputLength)]
        
        let multiSeedMap (seedRanges: SeedRange list): SeedRange list =
            seedRanges |> List.map singleSeedMap |> List.concat

        multiSeedMap

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

    let rec applyMaps (maps: Map list) (seedRanges: SeedRange list) : SeedRange list =
        match maps with
        | firstMap::restMaps -> (applyMaps restMaps (firstMap seedRanges))
        | [] -> seedRanges

    let processAlmanac (almanac: Almanac): Seed =
        let applyTheseMaps = applyMaps almanac.maps
        almanac.seeds 
            |> List.map (fun seed -> [(seed, int64 1)]) 
            |> List.map applyTheseMaps 
            |> List.concat
            |> List.map (fun (seed, _) -> seed)
            |> List.min

    let processAlmanacRanges (almanac: Almanac): Seed =
        let applyTheseMaps = applyMaps almanac.maps
        let Second (_, b) = b
        let ListToTuple = function 
            | [a; b] -> (a, b)
            | _ -> raise (AdventError "hmmm")
        almanac.seeds 
            |> List.indexed 
            |> List.groupBy (fun (ind, _) -> ind / 2) 
            |> List.map Second 
            |> List.map (List.map Second) 
            |> List.map ListToTuple
            |> applyTheseMaps 
            |> List.map (fun (seed, _) -> seed)
            |> List.min
