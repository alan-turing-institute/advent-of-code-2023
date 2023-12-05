using ProgressBars
using Test

test_input = readlines("test_input.txt")
input = readlines("input.txt")

function parse_input(input::Vector{String}, part_two::Bool=false)::Dict{String, Any}
    # parse the input to get the seeds (as ranges if part_two), the maps and the order of the maps
    # maps are saved as a dictionary where the keys are the source ranges and 
    # the values are the differences you must add to get the destination
    maps = Dict{String, Dict{UnitRange{Int64}, Int64}}()
    # initialize to empty
    seeds = Vector{Int64}()
    map_name = ""
    maps_in_order = Vector{Union{String, SubString}}()

    for line in input
        if occursin("seeds: ", line)
            seeds = parse.(Int64, split(split(input[1], ": ")[2], " "))
            seeds = part_two ? [seeds[i]:seeds[i]+seeds[i+1] for i in 1:2:length(seeds)-1] : seeds
        elseif occursin("map:", line)
            # line of input giving a new map name
            map_name = rstrip(line, [' ', 'm', 'a', 'p', ':'])
            # initialise a new map
            maps[map_name] = Dict{Int64, Int64}()
            # add the map name to the list of maps in order
            push!(maps_in_order, map_name)
        elseif line == ""
            # blank line, skip
            continue
        else
            # line of input giving the numbers and range
            destination_start, source_start, range_length = parse.(Int64, split(line, " "))
            source_range = source_start:(source_start+range_length-1)
            merge!(maps[map_name], Dict(source_range => destination_start-source_start))
        end
    end

    return Dict("seeds" => seeds, "maps" => maps, "maps_in_order" => maps_in_order)
end

function ☠️(input::Vector{String}, part_two::Bool=false)::Int64
    # parse the input to get the seeds and maps
    parsed_input = parse_input(input, part_two)
    current_min = Inf64

    # if part_two, collect and reduce the seed ranges
    parsed_input["seeds"] = part_two ? reduce(vcat, [collect(x) for x in parsed_input["seeds"]]) : parsed_input["seeds"]

    # for each seed, convert it using the maps and keep track of the minimum
    for seed in ProgressBar(parsed_input["seeds"])
        converted_number = seed
        for map_name in parsed_input["maps_in_order"]
            for (source_range, difference) in parsed_input["maps"][map_name]
                if converted_number in source_range
                    converted_number += difference
                    break
                end
            end
        end
        if converted_number < current_min
            current_min = converted_number
        end
    end

    return current_min
end

function is_valid_seed(seed::Int64, seed_ranges::Vector{UnitRange{Int64}})::Bool
    # check if a seed is valid by checking if it's in any of the seed ranges
    for seed_range in seed_ranges
        if seed in seed_range
            return true
        end
    end
    return false
end

function part_two(input::Vector{String})::Int64
    # start from a final location and go backwards in the processes
    # keep increasing the final location until we find a valid starting seed
    
    # parse the input to get the seeds and maps
    parsed_input = parse_input(input, true)
    final_location = 0
    while true
        # run the process backwards
        converted_number = final_location
        for map_name in reverse(parsed_input["maps_in_order"])
            for (source_range, difference) in parsed_input["maps"][map_name]
                if converted_number-difference in source_range
                    converted_number -= difference
                    break
                end
            end
        end
        
        # if it's a valid seed, break and return
        if is_valid_seed(converted_number, parsed_input["seeds"])
            return final_location
        end

        # continue until we find a valid seed
        final_location += 1
    end
end

# testing cases
Test.@test ☠️(test_input, false) == 35
Test.@test ☠️(test_input, true) == 46
Test.@test part_two(test_input) == 46

# Part One
println("Part One: ", ☠️(input, false))

# Part Two
# ☠️(input, true) would probably take 46 hours ☠️☠️☠️
println("Part Two: ", part_two(input))
