open System.IO

let testInput1 = 
    "1abc2
pqr3stu8vwx
a1b2c3d4e5f
treb7uchet".Split "\n"

let input = File.ReadAllLines "day-01/fsharp_evelinag/input.txt"

let calibrationValues (input: string []) = 
    input 
    |> Array.map (fun line -> 
        line.ToCharArray() 
        |> Array.filter (fun x -> 
            x >= '0' && x <= '9') 
        )
    |> Array.map (fun ns ->
        string ns.[0] + string (Array.last ns)
        |> int)

let part1 =
    calibrationValues input 
    |> Array.sum

// Part 2

let testInput2 = 
    "two1nine
eightwothree
abcone2threexyz
xtwone3four
4nineeightseven2
zoneight234
7pqrstsixteen".Split "\n"

let matchNumber (xs: string) =
    if xs.StartsWith "zero" then Some "0"
    else if xs.StartsWith "one" then Some "1"
    else if xs.StartsWith "two" then Some "2"
    else if xs.StartsWith "three" then Some "3"
    else if xs.StartsWith "four" then Some "4"
    else if xs.StartsWith "five" then Some "5"
    else if xs.StartsWith "six" then Some "6"
    else if xs.StartsWith "seven" then Some "7"
    else if xs.StartsWith "eight" then Some "8"
    else if xs.StartsWith "nine" then Some "9"
    else None

let recogniseNumbers (xs: string) =
    [| 0..xs.Length-1 |] 
    |> Array.map (fun i ->
        match matchNumber xs.[i..] with
        | Some n -> n
        | None -> string xs.[i])
    |> String.concat ""

let part2 = 
    input
    |> Array.map recogniseNumbers
    |> calibrationValues
    |> Array.sum

printfn "%A" part2