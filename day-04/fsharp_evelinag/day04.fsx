open System.IO

let input = File.ReadAllLines "input.txt"

let testInput = 
    "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11".Split "\n"

let cardsText = input

let parseCard (line: string) =
    line.Split ":"
    |> fun a -> a.[1].Split "|"
    |> Array.map (fun xs -> 
        xs.Split " "
        |> Array.filter ((<>) "")
        |> Array.map int)
    |> fun numbers -> 
        numbers.[0] |> Set, numbers.[1]

let cards = cardsText |> Array.map parseCard

let cardScores =
    cards
    |> Array.mapi (fun cardIdx (winningNumbers, myNumbers) ->
        let count =
            myNumbers
            |> Array.filter (fun n -> winningNumbers.Contains n)
            |> Array.length
        cardIdx + 1, count )


let part1 = 
    cardScores
    |> Array.sumBy (fun (cardIdx, count) ->
        if count = 0 then
            0
        else 
            pown 2 (count - 1))

printfn $"{part1}"

//--------

let initialCards = cardScores |> Array.map fst |> List.ofArray
let scoreLookup = cardScores |> dict

let rec playPart2 (cards: int list) total = 
    match cards with
    | [] -> total
    | cardNumber::xs ->
        let cardCopies = 
            [ 1 .. scoreLookup.[cardNumber] ] 
            |> List.map (fun i -> cardNumber + i)
        playPart2 (List.append cardCopies xs) (total + cardCopies.Length)

let part2 = playPart2 initialCards cards.Length

printfn $"{part2}"