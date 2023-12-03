using Test

test_input = readlines("test_input.txt")
test_input_p2 = readlines("test_input_2.txt")
input = readlines("input.txt")

function obtain_calibration_line(line)
    # filter the numebrs in the line
    numbers = filter(isdigit, collect(line))
    # concatenate first and last digits
    return parse(Int64, first(numbers) * last(numbers))
end

function part_one(document)
    return sum([obtain_calibration_line(line) for line in document])
end

function fix_line(line::String)
    # deal with some edge cases where two numbers share character
    return replace(line,
        "twone" => "twoone",
        "oneight" => "oneeight",
        "threeight" => "threeeight",
        "fiveight" => "fiveeight",
        "sevenine" => "sevennine",
        "eightwo" => "eighttwo",
        "eighthree" => "eightthree")
end

function replace_numbers(line)
    # add characters where two numbers share character
    line = fix_line(line)
    # replace numbers with digits
    return replace(line,
        "one" => "1",
        "two" => "2",
        "three" => "3",
        "four" => "4",
        "five" => "5",
        "six" => "6",
        "seven" => "7",
        "eight" => "8",
        "nine" => "9")
end

function part_two(document)
    return sum([obtain_calibration_line(replace_numbers(line)) for line in document])
end

# testing cases
Test.@test part_one(test_input) == 142
Test.@test part_two(test_input_p2) == 281

# Part One
println("Part One: ", part_one(input))

# Part Two
println("Part Two: ", part_two(input))
