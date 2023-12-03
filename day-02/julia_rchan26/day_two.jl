using Test

test_input = readlines("test_input.txt")
input = readlines("input.txt")

function check_colours(
    game_record::Union{SubString,String},
    available::Dict{String,Int64},
)::Bool
    grabs = split(game_record, "; ")
    for grab in grabs
        for (quantity, colour) in [split(x, " ") for x in split(grab, ", ")]
            if parse(Int64, quantity) > available[colour]
                return false
            end
        end
    end
    return true
end

function obtain_minimum_required(game_record::Union{SubString,String})::Dict{String,Int64}
    minimum_required = Dict{String,Int64}("red" => 0, "green" => 0, "blue" => 0)
    grabs = split(game_record, "; ")
    for grab in grabs
        for (quantity, colour) in [split(x, " ") for x in split(grab, ", ")]
            if parse(Int64, quantity) > minimum_required[colour]
                minimum_required[colour] = parse(Int64, quantity)
            end
        end
    end
    return minimum_required
end

function part_one(input::Vector{String})::Int64
    total = 0
    for line in input
        game, game_record = split(line, ": ")
        available = Dict{String,Int64}("red" => 12, "green" => 13, "blue" => 14)
        if check_colours(game_record, available)
            game_id = parse(Int64, split(game, " ")[2])
            total += game_id
        end
    end
    return total
end

function part_two(input::Vector{String})::Int64
    total = 0
    for line in input
        _, game_record = split(line, ": ")
        minimum_required = obtain_minimum_required(game_record)
        total += prod(values(minimum_required))
    end
    return total
end

# testing cases
Test.@test part_one(test_input) == 8
Test.@test part_two(test_input) == 2286

# Part One
println("Part One: ", part_one(input))

# Part Two
println("Part Two: ", part_two(input))
