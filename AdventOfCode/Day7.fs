module AdventOfCode.Day7

type Hand = string
type Bid = int
type HandBid = Hand*Bid

let parseLine (line: string): HandBid =
    let parts: string list = line.Split " " |> Seq.toList
    match parts with
        | [hand; bid] -> (hand, int bid)
        | _ -> raise (Common.AdventError "wtf")

let parseInput (input: string list): HandBid list = 
    input |> List.map parseLine

let charCountToHandType (charCounts: int seq): int =
    match charCounts |> Seq.toList with
    | [5] -> 0
    | [4; 1] -> 1
    | [3; 2] -> 2
    | [3; 1; 1] -> 3
    | [2; 2; 1] -> 4
    | [2; 1; 1; 1] -> 5
    | _ -> 6

let handType (hand: Hand): int =
    let charCounts = 
        hand 
        |> Seq.countBy id 
        |> Seq.map snd 
        |> Seq.sortDescending

    if charCounts |> Seq.fold (+) 0 <> 5 then
        raise (Common.AdventError "invalid hand")
    
    charCountToHandType charCounts

let cardStrength (c: char): int =
    let strengths = "AKQJT98765432"
    let res = strengths.IndexOf(c)
    if res = -1 then   
        raise (Common.AdventError "unknown card")
    res

let compareHands (hand1: Hand) (hand2: Hand): int =
    let t = compare (handType hand1) (handType hand2)
    if t <> 0 then t else
    try 
        Seq.map2 (fun a b -> compare (cardStrength a) (cardStrength b)) hand1 hand2
            |> Seq.find (fun c -> c <> 0)
    with 
    | :? System.Collections.Generic.KeyNotFoundException as _ -> 0

let sortHands (hands: Hand seq): Hand seq =
    hands |> Seq.sortWith compareHands |> Seq.rev

let part1 (hands: HandBid seq): int =
    let sorted = hands |> Seq.map fst |> sortHands
    let rec proc (ind: int, rest: Hand seq): int list = 
        if Seq.isEmpty rest then [] else
        let head: Hand = rest |> Seq.head
        let bid = hands |> Seq.find (fun (h, _) -> h = head) |> snd
        (ind * bid) :: proc (ind + 1, rest |> Seq.tail)
    proc (1, sorted) |> Seq.fold (+) 0

// Part 2 

let handType2 (hand: Hand): int =
    let jockers = hand |> Seq.filter (fun c -> c = 'J') |> Seq.length
    let charCounts = 
        hand.Replace("J", "")
        |> Seq.countBy id 
        |> Seq.map snd 
        |> Seq.sortDescending
        |> (fun c -> if Seq.isEmpty c then
                        seq [jockers]
                     else
                        Seq.append [jockers + (Seq.head c)] (Seq.tail c))

    if charCounts |> Seq.fold (+) 0 <> 5 then
        raise (Common.AdventError "invalid hand")
    
    charCountToHandType charCounts

let cardStrength2 (c: char): int =
    let strengths = "AKQT98765432J"
    let res = strengths.IndexOf(c)
    if res = -1 then   
        raise (Common.AdventError "unknown card")
    res

let compareHands2 (hand1: Hand) (hand2: Hand): int =
    let t = compare (handType2 hand1) (handType2 hand2)
    if t <> 0 then t else
    try 
        Seq.map2 (fun a b -> compare (cardStrength2 a) (cardStrength2 b)) hand1 hand2
            |> Seq.find (fun c -> c <> 0)
    with 
    | :? System.Collections.Generic.KeyNotFoundException as _ -> 0

let part2 (hands: HandBid seq): int =
    let sorted = hands |> Seq.map fst |> Seq.sortWith compareHands2 |> Seq.rev
    let rec proc (ind: int, rest: Hand seq): int list = 
        if Seq.isEmpty rest then [] else
        let head: Hand = rest |> Seq.head
        let bid = hands |> Seq.find (fun (h, _) -> h = head) |> snd
        (ind * bid) :: proc (ind + 1, rest |> Seq.tail)
    proc (1, sorted) |> Seq.fold (+) 0