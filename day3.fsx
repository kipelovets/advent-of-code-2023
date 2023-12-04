open System.IO

#load "Common.fs"

let sampleInput =
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

let hasAdjacentSymbol (input: string array)(x: int)(y: int) : bool =
    let check (x1: int, y1: int): bool =
        input[y1][x1] |> isSymbol
    let checkCol (x1: int) : bool =
        (y > 0 && check(x1, y - 1)) ||
            check(x1, y) ||
            (y < (input.Length - 1) && check(x1, y + 1))

    (x > 0 && checkCol(x - 1)) 
        || checkCol(x) 
        || (x < input[0].Length - 1 && checkCol(x + 1))

let findPartNumbersRow (input: string array)(y: int): int seq =
    let rec checkRowRest (input: string array)(x: int)(y: int)(numStart: int)(curNumGood: bool): int seq =
        let row = input[y]
        let len = row.Length
        if x > len - 1 then
            if curNumGood then
                seq { int row[numStart..(len - 1)] }
            else 
                Seq.empty
        else
            if isDigit(row[x]) then
                checkRowRest input (x + 1) y (if numStart <> -1 then numStart else x) ((if numStart <> -1 then curNumGood else false) || (hasAdjacentSymbol input x y)) 
            else 
                Seq.concat [
                    (if numStart <> -1 && curNumGood then seq { (int (row[numStart..(x - 1)])) } else Seq.empty)  ;
                    (checkRowRest input (x + 1) y -1 false) ]

    checkRowRest input 0 y -1 false

let partNumberSum (input: string array): int =
    seq { for y in 0..(input.Length - 1) do yield! (findPartNumbersRow input y) } |> Seq.fold (+) 0

partNumberSum sampleInput |> printfn "Day3.1 sample: %d"

let realInput = "day3.input" |> File.ReadAllText |> Common.SplitInput

partNumberSum realInput |> printfn "Day3.1: %d"
