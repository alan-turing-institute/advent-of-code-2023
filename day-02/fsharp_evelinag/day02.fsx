open System.IO

let input = File.ReadAllLines "day-02/fsharp_evelinag/input.txt"

let testInput = 
    "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green".Split "\n"

let getCount (cubes: (string*int)[]) colour =
    cubes
    |> Array.filter (fun (name, n) -> name = colour)
    |> fun x -> 
        if x.Length = 0 then 0
        else snd x.[0]

// Colours in order R-G-B
let parseGame (line: string) =
    let parts = line.Split ":"
    let gameNumber = 
        parts.[0].Split " " 
        |> Array.item 1 |> int
    let cubeCounts = 
        parts.[1]
        |> fun s -> s.Split ";"
        |> Array.map (fun (draw : string) ->
            let cubes =
                draw.Split ","
                |> Array.map (fun colourCounts ->
                    colourCounts.Trim().Split " "
                    |> fun xs -> xs.[1], int xs.[0])
            [| getCount cubes "red"; getCount cubes "green"; getCount cubes "blue" |]
            ) 
    gameNumber, cubeCounts

let games = 
//   testInput
    input
    |> Array.map parseGame

// 12 red cubes, 13 green cubes, and 14 blue cubes
let limits = [|12; 13; 14|]

let countPossible = 
    games
    |> Array.choose (fun (gameId, draws) ->
        let possible =
            draws 
            |> Array.filter (fun draw ->
                (draw, limits)
                ||> Array.zip 
                |> Array.fold (fun isImpossible (d, l) -> isImpossible || d > l) false)
            |> Array.isEmpty
        if possible then 
            Some gameId 
        else 
            None
        )
    |> Array.sum 

// part 1
countPossible  

let requiredCount (draws: int [][]) (colour: int) = 
    draws
    |> Array.map (fun draw -> draw.[colour])
    |> Array.max


let powers =
    games
    |> Array.map (fun (gameId, draws) ->
            requiredCount draws 0 
            * requiredCount draws 1
            * requiredCount draws 2)

// part 2    
powers    
|> Array.sum