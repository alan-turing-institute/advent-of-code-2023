using Test

test_input = readlines("test_input.txt")
input = readlines("input.txt")

function parse_card(card::String)::Tuple{Int64,Vector{Int64},Vector{Int64}}
    card_id, numbers = split(card, ": ")
    card_number = parse(Int64, last(split(card_id, " ")))
    winning, numbers_you_have = split(numbers, " | ")
    winning = [parse(Int64, x) for x in split(winning, " ") if x != ""]
    numbers_you_have = [parse(Int64, x) for x in split(numbers_you_have, " ") if x != ""]
    return card_number, winning, numbers_you_have
end

function compute_card_worth(
    card::String,
    winning_numbers::Union{Vector{Int64},Nothing} = nothing,
)::Int64
    _, winning, numbers_you_have = parse_card(card)
    if isnothing(winning_numbers)
        winning_numbers = intersect(winning, numbers_you_have)
    end
    if length(winning_numbers) == 0
        return 0
    elseif length(winning_numbers) == 1
        return 1
    else
        return 2^(length(winning_numbers) - 1)
    end
end

function part_one(pile_of_cards::Vector{String})::Int64
    return sum(compute_card_worth.(pile_of_cards))
end

function obtain_copies_count(pile_of_cards::Vector{String})::Dict{Int64,Int64}
    card_copies_counter = Dict{Int64,Int64}(i => 1 for i = 1:length(pile_of_cards))
    for i = 1:(length(pile_of_cards)-1)
        _, winning, numbers_you_have = parse_card(pile_of_cards[i])
        winning_numbers = intersect(winning, numbers_you_have)
        from = min(i + 1, length(pile_of_cards))
        to = min(i + length(winning_numbers), length(pile_of_cards))
        for j = from:to
            card_copies_counter[j] += card_copies_counter[i]
        end
    end
    return card_copies_counter
end

function part_two(pile_of_cards::Vector{String})::Int64
    copies_count = obtain_copies_count(pile_of_cards)
    return sum(values(copies_count))
end

# testing cases
Test.@test part_one(test_input) == 13
Test.@test part_two(test_input) == 30

# Part One
println("Part One: ", part_one(input))

# Part Two
println("Part Two: ", part_two(input))
