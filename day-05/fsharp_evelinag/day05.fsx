open System.IO

let input = File.ReadAllText "input.txt"

let testInput = 
    "seeds: 79 14 55 13

seed-to-soil map:
50 98 2
52 50 48

soil-to-fertilizer map:
0 15 37
37 52 2
39 0 15

fertilizer-to-water map:
49 53 8
0 11 42
42 0 7
57 7 4

water-to-light map:
88 18 7
18 25 70

light-to-temperature map:
45 77 23
81 45 19
68 64 13

temperature-to-humidity map:
0 69 1
1 0 69

humidity-to-location map:
60 56 37
56 93 4"

let almanach = input

// parse input
let parts = almanach.Split "\n\n"

let seeds = 
    parts.[0].Split ":" 
    |> Array.item 1 
    |> fun s -> s.Trim().Split " " 
    |> Array.map int64


let rangeMaps = 
    parts.[1..]
    |> Array.map (fun m ->
        m.Split "\n"
        |> fun a -> a.[1..]
        |> Array.map (fun line -> 
            line.Split " " 
            |> Array.map int64))


// -------------

let applyMap (map: (int64[])[]) seed = 
    let mappedSeed = 
        map 
        |> Array.choose (fun m -> 
                    if seed >= m.[1] && seed <= m.[1] + m.[2] - 1L then
                        Some (m.[0] + (seed - m.[1]))
                    else 
                        None)
    if mappedSeed.Length = 1 then mappedSeed.[0] else seed
    
// Run a seed through all the mapping steps
let mapSeed (seed: int64) =
    rangeMaps
    |> Array.fold (fun seed' maps' -> applyMap maps' seed') seed

let part1 =
    seeds
    |> Array.map mapSeed
    |> Array.min

printfn $"{part1}"    

// -------------

// interpret seeds as a range
let seedsRange = 
    seeds 
    |> Array.chunkBySize 2 
    |> Array.map (fun xs -> xs.[0], xs.[1])


// apply mappings from a given step to a seed interval
let applyRangeMap (maps: int64[][]) (seedStart : int64, seedLength: int64) =

    // split into intervals that need to be treated separately
    let intervalBreakpoints = 
        [| seedStart; seedStart + seedLength |]
        |> Array.append (
                maps 
                |> Array.collect (fun (map: int64[]) -> [| map.[1]; map.[1] + map.[2]|]) 
        )
        |> Array.distinct
        |> Array.sort 
        |> Array.pairwise
        |> Array.map (fun (x, y) -> x, y - x ) // back into (start, length) format for each  interval
        |> Array.filter (fun (x, _) -> x >= seedStart && x < (seedStart + seedLength)) // filter out intervals outside of seed range

    // now go through the intervals and apply the mappings, reusing applyMap from part 1
    intervalBreakpoints
    |> Array.map (fun (x, length) -> 
        applyMap maps x, length)
    
// Run a seed range through all the mapping steps    
let mapSeedRange (seed: int64 * int64) =
    rangeMaps
    |> Array.fold (fun seedIntervals maps' -> 
        seedIntervals
        |> Array.collect (fun seed -> applyRangeMap maps' seed)) [| seed |]

let part2 = 
    seedsRange
    |> Array.collect mapSeedRange
    |> Array.minBy fst
    |> fst

printfn $"{part2}"