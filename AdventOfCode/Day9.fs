module AdventOfCode.Day9

type Input = int seq seq

let parseInput (input: string seq): Input = 
    input |> Seq.map (fun line -> line.Split(" ") |> Seq.map (int))

let differences (line: int seq): int seq =
    line |> Seq.pairwise |> Seq.map (fun (a, b) -> b - a)

let nonzero a = a <> 0

let calcAllDiffs (line: int seq): int seq seq =
    let rec calcDiffs (cur: int seq seq): int seq seq =
        let last = cur |> Seq.head
        if (last |> Seq.filter nonzero |> Seq.length) = 0 then
            cur
        else
            calcDiffs (Seq.append (seq {differences last}) cur)
    
    calcDiffs [line]

let predictNext (line: int seq): int =
    calcAllDiffs line
    |> Seq.map Seq.last 
    |> Seq.fold (+) 0

let predictAll (predictor: int seq -> int) (lines: Input): int =
    lines
    |> Seq.map predictor
    |> Seq.fold (+) 0

let predictPrev (line: int seq): int =
    calcAllDiffs line
    |> Seq.map Seq.head
    |> Seq.fold (fun a b -> b - a) 0