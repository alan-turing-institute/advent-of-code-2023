import itertools
 
# read the input file
with open('input03.txt') as f:
    lines = f.read().splitlines()

## part 1
total = 0
for row, line in enumerate(lines):
    # numbers can span multiple columns, have to construct the full number first
    num = ""
    # by default assume there are no adjacent symbols, check for each digit whether there are any
    adjacent = False
    for col, char in enumerate(line):
        # check for numbers
        if char.isdigit():
            num += char
            # if have already found an adjacent symbol, no need to do anything else
            if adjacent:
                continue
            # otherwise check adjacent positions for symbols (including diagonally)
            else:
                for i,j in itertools.product([0, 1, -1], [0, 1, -1]):
                    # make sure we are within grid limits, rows are lines and columns are within a line
                    if row+i>=0 and row+i<len(lines) and col+j>=0 and col+j<len(line):
                        # is there a symbol (i.e., not a digit or ".") at this position?
                        adj_char = lines[row+i][col+j]
                        if not adj_char.isdigit() and adj_char != ".":
                            adjacent = True
                            # break
        # check if have a number to add to the total
        else:
            if num != "":
                if adjacent:
                    total += int(num)
                num = ""
                adjacent = False
    # the line might have finished with a number, add this to the total
    if num != "":
        if adjacent:
            total += int(num)

print(total)

## part 2

# list of lists identifying each number and corresponding indexes: [[num, [[row, col],...]], ...]
number_data = []
# [[row, col], ...] of each * symbol
symbol_pos = []

for row, line in enumerate(lines):
    num = ""
    num_idx = []
    for col, char in enumerate(line):
        if char.isdigit():
            num += char
            num_idx.append([row, col])
        else:
            if num != "":
                number_data.append([int(num), num_idx])
                num = ""
                num_idx = []
            if char == "*":
                symbol_pos.append([row, col])
    if num != "":
        number_data.append([int(num), num_idx])
    
total = 0
for pos_symbol in symbol_pos:
    adj_numbers = []
    for num,num_pos in number_data:
        adjacent = False
        for row,col in num_pos:
            for i,j in itertools.product([0, 1, -1], [0, 1, -1]):
                if [row+i, col+j]==pos_symbol:
                    adjacent = True
                    break
        if adjacent:
            adj_numbers.append(num)

    if len(adj_numbers) == 2:
        total += (adj_numbers[0] * adj_numbers[1])

print(total)
                        