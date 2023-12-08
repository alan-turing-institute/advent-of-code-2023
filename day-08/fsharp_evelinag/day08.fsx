open System.IO

let input = File.ReadAllText "input.txt"

let testInput = 
    "LR

11A = (11B, XXX)
11B = (XXX, 11Z)
11Z = (11B, XXX)
22A = (22B, XXX)
22B = (22C, 22C)
22C = (22Z, 22Z)
22Z = (22B, 22B)
XXX = (XXX, XXX)"

let parseInput (text: string) = 
    let parts = text.Split "\n\n"
    let instructions = parts.[0]
    let neighbours = 
        parts.[1].Split "\n"
        |> Array.map (fun line ->
            let parts' = line.Split "="
            let node = parts'.[0].Trim()
            let ns = parts'.[1].Split ","
            let left = ns.[0].Trim(')', '(', ' ')
            let right = ns.[1].Trim(')', '(', ' ')
            node, [ ('L', left); ('R', right) ] |> Map
            )
        |> dict
    instructions, neighbours


let instructions, network = parseInput input

let rec traverse node steps isFinished =
    if isFinished node then
        steps
    else
        let instr = instructions.[steps % instructions.Length]
        traverse network.[node].[instr] (steps + 1) isFinished

let part1 = traverse "AAA" 0 (fun x -> x = "ZZZ")      
printfn $"{part1}"

// Euclidean algorithm to find the greatest common divisor 
let rec gcd a b =
    if b = 0L then a
    else gcd b (a % b)    

// least common multiple
let lcm a b =
  (a * b) / (gcd a b) 

let initialNodes = 
    network.Keys
    |> Seq.toArray
    |> Array.filter (fun n -> n.[2] = 'A')

let traverseAllGhosts (nodes: string []) =
    nodes
    |> Array.map (fun node -> 
        traverse node 0 (fun x -> x.[2] = 'Z') |> int64)
    |> Array.fold lcm 1L


let part2 = traverseAllGhosts initialNodes
printfn $"{part2}"  