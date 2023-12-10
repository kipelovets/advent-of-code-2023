namespace AdventOfCode

module Day4 =
    type Card = int array * int array

    let parseInputLine (line: string): Card = 
        let cardParts = line.Split ":" 
        let numberLists = (cardParts[1]).Split " | " 
        let convertNumberList (numbers: string): int array =
            (numbers.Trim()).Split " " |> Array.filter (fun s -> s.Length > 0) |> Array.map int

        (convertNumberList numberLists[0], convertNumberList numberLists[1])


    let sampleInput: Card array =
         "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11" 
        |> Common.SplitInput 
        |> Array.map parseInputLine


    let cardWinningNumbers (card: Card): int =
        let (winner, owned) = card
        (Set.intersect (winner |> Set.ofArray) (owned |> Set.ofArray)).Count

    let cardValue (card: Card): int =
        let wonNumbers = cardWinningNumbers card
        if wonNumbers > 0 then
            pown 2 (wonNumbers - 1)
        else
            0

    let cardPileValue (cards: Card array): int =
        cards |> Array.map cardValue |> Array.fold (+) 0


    type CardRecord = int * int
    let totalWonCards (cards: Card list): int =
        let cardRecords = cards 
                          |> List.map (cardWinningNumbers >> fun x -> (x, 1))

        let rec calc (records: CardRecord list): int list =
            match records with
            | first::rest -> 
                let (wins, cnt) = first
                let restWithWins = (rest[..wins-1] 
                                        |> List.map (fun (w, c) -> (w, c + cnt))) @
                                   (rest[wins..])

                cnt::(calc restWithWins)
            | [] -> []
        
        calc cardRecords |> List.fold (+) 0
