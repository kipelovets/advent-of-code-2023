namespace AdventOfCode

open System.Text.RegularExpressions

module Day3 =
    type Schematic = string array

    let sampleInput: Schematic =
         "467..114..
    ...*......
    ..35..633.
    ......#...
    617*......
    .....+.58.
    ..592.....
    ......755.
    ...$.*....
    .664.598.." |> Common.SplitInput

    let isSymbol (c: char): bool =
        @"!@#$%^&*()-=_+[]{};':"",\/<>?".Contains(c)

    let isDigit (c: char): bool =
        @"0123456789".Contains(c)

    let hasAdjacentSymbol (input: Schematic)(x: int)(y: int) : bool =
        let check (x1: int, y1: int): bool =
            input[y1][x1] |> isSymbol
        let checkCol (x1: int) : bool =
            (y > 0 && check(x1, y - 1)) ||
                check(x1, y) ||
                (y < (input.Length - 1) && check(x1, y + 1))

        (x > 0 && checkCol(x - 1)) 
            || checkCol(x) 
            || (x < input[0].Length - 1 && checkCol(x + 1))

    let findPartNumbersRow (input: Schematic)(y: int): (int*int) seq =
        let rec checkRowRest (x: int)(numStart: int)(curNumGood: bool): (int*int) seq =
            let row = input[y]
            let len = row.Length
            if x > len - 1 then
                if curNumGood then
                    seq { (numStart, len - 1) }
                else 
                    Seq.empty
            else
                if isDigit(row[x]) then
                    checkRowRest (x + 1) (if numStart <> -1 then numStart else x) ((if numStart <> -1 then curNumGood else false) || (hasAdjacentSymbol input x y)) 
                else 
                    Seq.concat [
                        (if numStart <> -1 && curNumGood then seq { (numStart, x - 1) } else Seq.empty)  ;
                        (checkRowRest (x + 1) -1 false) ]

        checkRowRest 0 -1 false

    let partNumberSum (input: Schematic): int =
        seq { for y in 0..(input.Length - 1) do 
                let partBoundaries = findPartNumbersRow input y 
                yield! (partBoundaries |> Seq.map (fun (x1, x2) -> int (input[y][x1..x2]))) 
        } |> Seq.fold (+) 0

    // Part 2

    let rec findNumberStart (input: Schematic)(x1: int)(y1: int) : int option =
        if x1 >= 0 && isDigit (input[y1][x1]) then
            match findNumberStart input (x1 - 1) y1 with
            | Some(x) -> Some(x)
            | None -> Some(x1)
        else
            None

    let extractNumber (input: Schematic)(x: int)(y: int) : int option =
        let rec extractString (input: Schematic)(x: int)(y: int) : string option =
            if x > input[y].Length - 1 || not (isDigit (input[y][x])) then
                None
            else 
                let curDigit = string (input[y][x])
                match extractString input (x + 1) y with
                | Some(rq) -> Some(curDigit + rq)
                | None -> Some(curDigit)

        match extractString input x y with
        | Some(x) -> Some(int x)
        | None -> None
            
    let gearTouchingNumbers (input: Schematic)(x: int)(y: int) : int list =
        let findNumbersOnRow (y1: int) : int list =
            seq { for x1 in (max 0 (x - 1))..(min (x + 1) (input[y1].Length - 1)) do
                    if isDigit (input[y1][x1]) then
                        yield findNumberStart input x1 y1 |> Option.get } 
                |> Seq.distinct |> Seq.map (fun x1 -> Option.get (extractNumber input x1 y1) ) |> Seq.toList
                

        (if y = 0 then [] else findNumbersOnRow (y - 1)) @
                                (findNumbersOnRow y) @
                                (if y = input.Length - 1 then [] else findNumbersOnRow (y + 1))

    let gearRatio (input: Schematic)(x: int)(y: int) : int =
        let touchingNumbers = gearTouchingNumbers input x y

        if touchingNumbers.Length = 2 then
            touchingNumbers[0] * touchingNumbers[1]
        else
            0

    let gearRatios (input: Schematic) : int =
        seq { for y in 0..input.Length - 1 do
                for x in 0..input[0].Length - 1 do
                    if input[y][x] = '*' then
                        let res = gearRatio input x y 
                        // printfn $"{x} {y} {res}"
                        yield res } |> Seq.fold (+) 0
