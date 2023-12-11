open System.IO

let input = File.ReadAllLines "input.txt"

let testInput = 
    "...#......
.......#..
#.........
..........
......#...
.#........
.........#
..........
.......#..
#...#.....".Split('\n')

let image = 
    input
    |> Array.map (fun row -> row.ToCharArray())
    
// which rows and columns to expand
let expandedRows =
    image
    |> Array.mapi (fun idx row -> 
        row 
        |> Array.distinct
        |> fun a -> if a.Length = 1 then Some idx else None)
    |>  Array.choose id
    |> Set

let expandedColumns =
    [| 0 .. image.[0].Length-1 |]
    |> Array.filter (fun idx ->
        image 
        |> Array.distinctBy (fun row -> row.[idx])
        |> fun a -> if a.Length = 1 then true else false
        )
    |> Set

let galaxyCoordinates =
    image
    |> Array.mapi (fun rowIdx row ->
        row
        |> Array.mapi (fun colIdx x -> 
            if x = '#' then Some(rowIdx, colIdx) else None))
    |> Array.concat
    |> Array.choose id

let calculateRowDistance expansionFactor r1 r2 = 
    [ min r1 r2 .. max r1 r2 ]
    |> List.map (fun r -> 
        if expandedRows.Contains r then expansionFactor else 1L)
    |> List.sum
    |> fun x -> x - 1L

let calculateColDistance expansionFactor c1 c2 = 
    [ min c1 c2 .. max c1 c2 ]
    |> List.map (fun c -> 
        if expandedColumns.Contains c then expansionFactor else 1L)
    |> List.sum
    |> fun x -> x - 1L


let distances1 = 
    [| for i in 0..galaxyCoordinates.Length-1 do 
        for j in 0..i-1 ->
            (galaxyCoordinates.[i], galaxyCoordinates.[j]) |]
    |> Array.map (fun ((rowIdx1, colIdx1), (rowIdx2, colIdx2)) -> 
        calculateRowDistance 2L rowIdx1 rowIdx2
        + calculateColDistance 2L colIdx1 colIdx2
        )
    |> Array.sum

let distances2 = 
    [| for i in 0..galaxyCoordinates.Length-1 do 
        for j in 0..i-1 ->
            (galaxyCoordinates.[i], galaxyCoordinates.[j]) |]
    |> Array.map (fun ((rowIdx1, colIdx1), (rowIdx2, colIdx2)) -> 
        calculateRowDistance 1000000L rowIdx1 rowIdx2
        + calculateColDistance 1000000L colIdx1 colIdx2
        )
    |> Array.sum


printfn $"{distances1}"
printfn $"{distances2}"
  