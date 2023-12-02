import re


# read the input file
with open('input01.txt') as f:
    lines = f.read().splitlines()

# part 1: find first and last digit on each line
total = 0
for line in lines:
    digits = list(''.join(re.findall(r'\d+', line)))
    two_digit = digits[0] + digits[-1]
    total += int(two_digit)

print("part 1:", total)

# part 2: find first and last digit on each line (including written numbers)
word_to_digit = {
        "one":"1", "two":"2", "three":"3", 
        "four":"4", "five":"5", "six":"6", 
        "seven":"7", "eight":"8", "nine":"9"
        }
word_nums = '|'.join(word_to_digit.keys() | word_to_digit.values())

total = 0
for line in lines:
    # find all digits and written numbers (include search for overlapping words !!)
    nums = re.findall(f"(?=({word_nums}))", line)
    # turn written numbers into string integers
    single_digits = [x if x not in word_to_digit else word_to_digit[x] for x in nums]
    # combine
    two_digit = single_digits[0] + single_digits[-1]
    total += int(two_digit)

print("part 2:", total)
