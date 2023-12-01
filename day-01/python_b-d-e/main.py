# advent of code - day 1
import re

# read input
with open('input.txt', 'r') as f:
    input_string = f.readlines()


def part_one(input_string):
    # ugly enumeration
    total = 0

    nums = [str(x) for x in[0,1,2,3,4,5,6,7,8,9]]

    for string in input_string:
        start = 0
        while string[start] not in nums:
            start += 1
        end = len(string) - 1
        while string[end] not in nums:
            end -= 1
        concat = string[start] + string[end]
        total += int(concat)

    print(total)


def part_two(input_string):
    # nicer (but possibly more intensive) regex solution
    one_digit_number_regex = re.compile(r"(?=(one|two|three|four|five|six|seven|eight|nine|\d))")
    num_ref = {"one":"1", "two":"2", "three":"3", "four":"4", "five":"5", "six":"6", "seven":"7", "eight":"8", "nine":"9"}
    total = 0

    for l in input_string:
        matches = one_digit_number_regex.findall(l)
        first = matches[0] if matches[0] not in num_ref else num_ref[matches[0]]
        last = matches[-1] if matches[-1] not in num_ref else num_ref[matches[-1]]
        total += int(first + last)

    print(total)

part_one(input_string)
part_two(input_string)