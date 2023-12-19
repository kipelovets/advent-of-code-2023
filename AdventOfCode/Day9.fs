module AdventOfCode.Day9

type Input = int seq seq

let parseInput (input: string seq): Input = 
    input |> Seq.map (fun line -> line.Split(" ") |> Seq.map (int))

let differences (line: int seq): int seq =
    line |> Seq.pairwise |> Seq.map (fun (a, b) -> b - a)

let nonzero a = a <> 0

let predictNext (line: int seq): int =
    let rec calcDiffs (cur: int seq seq): int seq seq =
        let last = cur |> Seq.head
        if (last |> Seq.filter nonzero |> Seq.length) = 0 then
            cur
        else
            calcDiffs (Seq.append (seq {differences last}) cur)
    
    calcDiffs [line]
    |> Seq.map Seq.last 
    |> Seq.fold (+) 0

let predictNextAll (lines: Input): int =
    lines
    |> Seq.map predictNext 
    |> Seq.fold (+) 0