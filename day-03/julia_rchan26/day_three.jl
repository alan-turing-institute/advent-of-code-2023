using Test

test_input = readlines("test_input.txt")
input = readlines("input.txt")

function obtain_full_number(
    schematic::Vector{String},
    x::Int,
    y::Int,
)::Union{Tuple{Nothing,Nothing},Tuple{Int64,Tuple{Int64,Int64,Int64}}}
    # for a given coordinate, obtain the full number
    # as well as the location of the number as (row, column_start, column_end)
    # if the coordinate is not a digit, return nothing in right format
    if !isdigit(schematic[x][y])
        return nothing, nothing
    end

    # initialize number
    number = schematic[x][y]

    # look left and pre-concatenate any digits
    left_index = y
    left_looking_y = y - 1
    while left_looking_y >= 1 && isdigit(schematic[x][left_looking_y])
        number = schematic[x][left_looking_y] * number
        left_index = left_looking_y
        left_looking_y -= 1
    end

    # look right and post-concatenate any digits
    right_index = y
    right_looking_y = y + 1
    while right_looking_y <= length(schematic[x]) && isdigit(schematic[x][right_looking_y])
        number *= schematic[x][right_looking_y]
        right_index = right_looking_y
        right_looking_y += 1
    end

    return parse(Int64, number), (x, left_index, right_index)
end

function obtain_adjacent_numbers(
    schematic::Vector{String},
    x::Int,
    y::Int,
)::Dict{Tuple{Int64,Int64,Int64},Int64}
    # for a given location, obtain all adjacent numbers
    # return as a dictionary where key is the location of the number
    # as (row, column_start, column_end) and value is the number
    adjacent_numbers = Dict{Tuple{Int64,Int64,Int64},Int64}()

    # check all adjacent coordinates
    for dx = -1:1
        # do not check outside the grid
        if x + dx < 1 || x + dx > length(schematic[1])
            continue
        end
        for dy = -1:1
            # do not check outside the grid
            if y + dy < 1 || y + dy > length(schematic)
                continue
            end
            if isdigit(schematic[x+dx][y+dy])
                number, location = obtain_full_number(schematic, x + dx, y + dy)
                if !isnothing(number)
                    adjacent_numbers[location] = number
                end
            end
        end
    end

    return adjacent_numbers
end

function is_symbol(c::Char)::Bool
    return !isdigit(c) && c != '.'
end

function part_one(schematic::Vector{String})::Int64
    part_numbers = Dict{Tuple{Int64,Int64,Int64},Int64}()
    for x = 1:length(schematic[1])
        for y = 1:length(schematic)
            if is_symbol(schematic[x][y])
                merge!(part_numbers, obtain_adjacent_numbers(schematic, x, y))
            end
        end
    end

    return sum(values(part_numbers))
end

function part_two(schematic::Vector{String})::Int64
    part_numbers = Vector{Int64}()
    for x = 1:length(schematic[1])
        for y = 1:length(schematic)
            if schematic[x][y] == '*'
                adjacent_numbers = obtain_adjacent_numbers(schematic, x, y)
                if length(adjacent_numbers) == 2
                    push!(part_numbers, prod(values(adjacent_numbers)))
                end
            end
        end
    end

    return sum(values(part_numbers))
end

# testing cases
Test.@test part_one(test_input) == 4361
Test.@test part_two(test_input) == 467835

# Part One
println("Part One: ", part_one(input))

# Part Two
println("Part Two: ", part_two(input))
