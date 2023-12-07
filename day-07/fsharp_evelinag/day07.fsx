open System.IO

let input = File.ReadAllLines "input.txt"

let testInput = 
    "32T3K 765
T55J5 684
KK677 28
KTJJT 220
QQQJA 483".Split "\n"

let parseInput (lines: string []) =
    lines
    |> Array.map (fun line -> 
        let parts = line.Split " "
        (parts.[0].ToCharArray(), int parts.[1]))

let cardsAndBids = parseInput input


// Value of each hand based on number of cards of the same type
let handValue (hand:char[]) =

    let counts = 
        hand
        |> Array.countBy id
        |> Array.map snd
        |> Array.sortDescending
        |> List.ofArray

    match counts with
    | [5] -> 6
    | [4; 1] -> 5
    | [3; 2] -> 4
    | [3; 1; 1] -> 3
    | [2; 2; 1] -> 2
    | [2; 1; 1; 1] -> 1
    | [1; 1; 1; 1; 1] -> 0
    | _ -> failwith $"{counts}"


let cardValueLookup = 
    ['A'; 'K'; 'Q'; 'J'; 'T'; '9'; '8'; '7'; '6'; '5'; '4'; '3'; '2']
    |> List.rev
    |> List.mapi (fun idx value -> value, idx)
    |> dict    

// Value of individual cards
let cardValue x = cardValueLookup.[x]

// Compare if hand1 < hand2 where hand1 and hand2 are of the same type
// i.e. compare individual cards in the hand
// Returns:
//      < 0 if hand1 < hand2, 
//      0 if hand1 = hand2 
//      > 0 if hand1 > hand2
let compareCards (hand1: char[]) (hand2: char[]) (cardValue : char -> int) : int = 
    (hand1, hand2)
    ||> Array.zip
    |> Array.fold (fun state (c1, c2) -> 
        if state <> 0 then state
        else cardValue c1 - cardValue c2
        ) 0

// Compare hands, first based on their type and then card-by-card
// Returns:
//      < 0 if hand1 < hand2, 
//      0 if hand1 = hand2 
//      > 0 if hand1 > hand2
let comparer ((hand1, bid1): char[] * int) ((hand2, bid2): char[] * int) =
    let value1 = handValue hand1 
    let value2 = handValue hand2
    if value1 = value2 then
       compareCards hand1 hand2 cardValue
    else 
        value1 - value2

let part1 = 
    cardsAndBids
    |> Array.sortWith comparer
    |> Array.mapi (fun rank (card, bid) -> (rank + 1) * bid)
    |> Array.sum

printfn $"{part1}"


// ------------------ Part 2 --------------------

let cardValueLookup2 =
    ['A'; 'K'; 'Q'; 'T'; '9'; '8'; '7'; '6'; '5'; '4'; '3'; '2'; 'J']
    |> List.rev
    |> List.mapi (fun idx value -> value, idx)
    |> dict 

let cardValue2 x = cardValueLookup2.[x]

let makeHandBetter (hand: char[]) =
    // find the most common card and replace J with that one
    let handCounts = 
        hand
        |> Array.filter (fun c -> c <> 'J')
        |> Array.countBy id
        |> Array.sortByDescending snd

    if handCounts.Length = 0 then 
        // 'JJJJJ' - do nothing, it's not possible to improve hand type
        hand
    else
        let mostCommon = fst handCounts.[0]
        hand 
        |> Array.map (fun c -> if c = 'J' then mostCommon else c)

let comparer2 ((hand1, bid1): char[] * int) ((hand2, bid2): char[] * int) =
    let value1 = makeHandBetter hand1 |> handValue  
    let value2 = makeHandBetter hand2 |> handValue
    if value1 = value2 then 
        // same hand type, compare cards
        compareCards hand1 hand2 cardValue2
    else
        value1 - value2

let part2 =
    cardsAndBids
    |> Array.sortWith comparer2
    |> Array.mapi (fun rank (card, bid) -> (rank + 1) * bid)
    |> Array.sum

printfn $"{part2}"  
