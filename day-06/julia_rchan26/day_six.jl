using Test

test_input = readlines("test_input.txt")
input = readlines("input.txt")

function parse_input(input::Vector{String}, part_two::Bool)::Dict{String, Union{Vector{Int64}, Int64}}
    # parse input into a dictionary
    # key is Distance, Time and values are either a vector of integers or a single integer (part II)
    sheet = Dict{String, Union{Vector{Int64}, Int64}}()
    for line in input
        key, value = split(line, ":")
        if part_two
            sheet[key] = parse(Int64, replace(value, " " => ""))
        else
            sheet[key] = [parse(Int64, x) for x in split(value, " ") if x != ""]
        end
    end
    return sheet
end

function 🏎️🏎️🏎️(hold_time::Int64, race_time::Int64)::Int64
    return hold_time*(race_time-hold_time)
end

function day_six(input::Vector{String}, part_two::Bool=false)::Int64
    sheet = parse_input(input, part_two)
    total = 1
    for i in 1:length(sheet["Time"])
        distance_travelled_per_hold = 🏎️🏎️🏎️.(1:sheet["Time"][i], sheet["Time"][i])
        total *= sum(distance_travelled_per_hold .> sheet["Distance"][i])
    end
    return total
end

# testing cases
Test.@test day_six(test_input) == 288
Test.@test day_six(test_input, true) == 71503

# Part One
println("Part One: ", day_six(input))

# Part Two
println("Part Two: ", day_six(input, true))
