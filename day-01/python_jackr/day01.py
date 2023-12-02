import re

with open("input.txt") as f:
    lines = f.readlines()

# PART 1
total = 0
for line in lines:
    digits_str = re.findall("[0-9]", line)
    first_last = int("".join([digits_str[0], digits_str[-1]]))
    total += first_last

print("Part 1", total)

# PART 2
patterns = [
    "one",
    "1",
    "two",
    "2",
    "three",
    "3",
    "four",
    "4",
    "five",
    "5",
    "six",
    "6",
    "seven",
    "7",
    "eight",
    "8",
    "nine",
    "9",
]
word_to_digit = {
    "one": "1",
    "two": "2",
    "three": "3",
    "four": "4",
    "five": "5",
    "six": "6",
    "seven": "7",
    "eight": "8",
    "nine": "9",
}


def pattern_in_str(string):
    for p in patterns:
        if p in string:
            if p in word_to_digit:
                return word_to_digit[p]
            return p
    return ""


total = 0
for line in lines:
    idx = 0
    first = ""
    while not first:
        first = pattern_in_str(line[:idx])
        idx += 1

    idx = -1
    last = ""
    while not last:
        last = pattern_in_str(line[idx:])
        idx -= 1
    first_last = int("".join([first, last]))
    total += first_last

print("Part 2", total)
