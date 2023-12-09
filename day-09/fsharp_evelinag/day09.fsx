open System.IO

let input = File.ReadAllLines "input.txt"

let testInput = 
    "0 3 6 9 12 15
1 3 6 10 15 21
10 13 16 21 30 45".Split('\n')

let oasis = 
    input
    |> Array.map (fun line -> line.Split " " |> Array.map int)

let rec getNext extractValue predict values =
    let allZeroes = 
        values 
        |> Array.filter ((<>) 0) 
        |> Array.isEmpty

    if allZeroes then
        0
    else
        let differences =
            values
            |> Array.pairwise
            |> Array.map (fun (t1, t2) -> t2 - t1)
        let x = getNext extractValue predict differences
        predict (extractValue values) x


let part1 = 
    oasis
    |> Array.map (getNext Array.last (+))
    |> Array.sum

printfn $"{part1}"   

let part2 = 
    oasis
    |> Array.map (getNext Array.head (-))
    |> Array.sum

printfn $"{part2}"   
