open System.IO

let input = File.ReadAllLines "input.txt"

let testInput = 
    "Time:      7  15   30
Distance:  9  40  200".Split "\n"

let parseInput input = 
    input
    |> Array.map (fun (line:string) -> 
        line.Split " " 
        |> Array.filter (fun x -> x <> "")
        |> fun a -> a.[1..] 
        |> Array.map int64)
    |> fun xs -> Array.zip xs.[0] xs.[1]

let raceTimes = parseInput input

let raceOptions (n: int64) (d: int64) =
    let n' = float n
    let d' = float d
    
    let tLower = 
        (n' - sqrt(n'*n' - 4. * d'))/2. 
        |> fun x -> x + 1.  // add 1 to make the inequality strict
        |> floor 

    let tUpper = 
        (n' + sqrt(n'*n' - 4. * d'))/2. 
        |> fun x -> x - 1. // subtract 1 to make the inequality strict
        |> ceil 

    int64 tUpper - int64 tLower + 1L


let part1 = 
    raceTimes
    |> Array.map (fun (time, distance) ->
        raceOptions time distance)
    |> Array.fold ( * ) 1L

let parseInput2 input = 
    input
    |> Array.map (fun (line:string) -> 
        line.Replace(" ", "").Split ":" 
        |> fun a -> int64 a.[1] )
    |> fun xs -> xs.[0], xs.[1]

let time2, distance2 = parseInput2 input

let part2 = raceOptions time2 distance2

printfn $"Part 1: {part1}"
printfn $"Part 2: {part2}"