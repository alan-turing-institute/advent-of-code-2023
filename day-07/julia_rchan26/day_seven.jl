using Test
using DataStructures

test_input = readlines("test_input.txt")
input = readlines("input.txt")

function parse_input(input::Vector{String})::OrderedDict{String, Int64}
    hands = SortedDict{String, Int64}()
    for line in input
        key, value = split(line, " ")
        hands[key] = parse(Int64, value)
    end
    return hands
end

function 🃏💪(🃏::Char, 🥈::Bool)::Int64
    # get card strength
    if 🃏 == 'A'
        return 14
    elseif 🃏 == 'K'
        return 13
    elseif 🃏 == 'Q'
        return 12
    elseif 🃏 == 'J'
        if 🥈
            return 1
        else
            return 11
        end
    elseif 🃏 == 'T'
        return 10
    else
        return parse(Int64, 🃏)
    end
end

function compare_draw(🖐️1::String, 🖐️2::String, 🥈::Bool)::Union{Bool,Nothing}
    # return true if 🖐️1 wins, false if 🖐️2 wins, nothing if draw
    # get the cards in each hand
    🖐️1_🃏🃏🃏 = only.(split(🖐️1, ""))
    🖐️2_🃏🃏🃏 = only.(split(🖐️2, ""))
    for i = 1:5
        # compare hand strenghts at location i
        🖐️1_🃏💪 = 🃏💪(🖐️1_🃏🃏🃏[i], 🥈)
        🖐️2_🃏💪 = 🃏💪(🖐️2_🃏🃏🃏[i], 🥈)
        if 🖐️1_🃏💪 > 🖐️2_🃏💪
            return true
        elseif 🖐️1_🃏💪 < 🖐️2_🃏💪
            return false
        end
    end
end

function 🆙🖐️(🖐️::String)::String
    # part_two: improve hand by replacing J with most common letter
    char_count = counter(🖐️)
    if (length(char_count) == 1) | !('J' in keys(char_count))
        # no improving a five of a kind or a hand with no J
        return 🖐️
    end

    # find the most common letter that is not a J
    most_common = reduce(
        (x, y) -> (x != 'J') & (char_count[x] >= char_count[y]) ? x : y,
        keys(char_count),
    )

    # replace any occurences of J with most common letter
    return replace(🖐️, "J" => most_common)
end

function 🖐️💪(🖐️::String, 🥈::Bool = false)::Int64
    # get hand strength
    if 🥈
        # improve the hand if possible
        🖐️ = 🆙🖐️(🖐️)
    end

    char_count = counter(🖐️)
    maximum_value = maximum(values(char_count))
    if length(char_count) == 1
        return 6
    elseif length(char_count) == 2
        if maximum_value == 4
            return 5
        else
            return 4
        end
    elseif length(char_count) == 3
        if maximum_value == 3
            return 3
        else
            return 2
        end
    elseif length(char_count) == 4
        return 1
    else
        return 0
    end
end

function compare_🖐️🖐️(🖐️1::String, 🖐️2::String, 🥈::Bool = false)::Union{Bool,Nothing}
    # return true if 🖐️1 wins
    🖐️1💪 = 🖐️💪(🖐️1, 🥈)
    🖐️2💪 = 🖐️💪(🖐️2, 🥈)
    if 🖐️1💪 > 🖐️2💪
        return true
    elseif 🖐️1💪 < 🖐️2💪
        return false
    else
        return compare_draw(🖐️1, 🖐️2, 🥈)
    end
end

function day_seven(input::Vector{String}, 🥈::Bool = false)::Int64
    hands = parse_input(input)
    sorted_hands = sort(hands, lt = (x, y) -> compare_🖐️🖐️(x, y, 🥈))
    return sum(reverse(collect(values(sorted_hands))) .* collect(1:length(sorted_hands)))
end

# testing cases
Test.@test day_seven(test_input) == 6440
Test.@test day_seven(test_input, true) == 5905

# Part One
println("Part One: ", day_seven(input))

# Part Two
println("Part Two: ", day_seven(input, true))
