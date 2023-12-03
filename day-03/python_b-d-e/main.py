# aoc day 3


def check_adjacent_symbol(data, x, y):
    # check if character above below left right or diagonal is not a . or number
    symbol = False
    ignore = ['.', '0', '1', '2', '3', '4', '5', '6', '7', '8', '9']
    grid_height = len(data)
    grid_width = len(data[0])
    # check full square around characted - ignore if off the grid
    for i in [-1, 0, 1]:
        for j in [-1, 0, 1]:
            if i!=0 or j!=0:
                # check if off grid
                if x+i < 0 or x+i >= grid_width or y+j < 0 or y+j >= grid_height:
                    continue
                # check if symbol
                if data[y+j][x+i] not in ignore:
                    symbol = True
                    break
    return symbol


def load_data(fname):
    # load txt into 2d array
    with open(fname) as f:
        data = f.readlines()
    # remove any empty lines from end
    while data[-1] == '\n':
        data.pop()
    for i in range(len(data)):
        data[i] = list(data[i].strip())
    return data


def part_one(data):

    total = 0

    for h in range(len(data)):
        row = data[h]
        on_number = False
        # current_number = None
        for i in range(len(row)):
            # check char is string digit
            char = row[i]
            if char.isdigit():
                if not on_number:
                    on_number = True
                    # get full number
                    j = i
                    while j<len(row) and row[j].isdigit():
                        j += 1
                    # check if any digit has adjacent symbol
                    for k in range(i, j):
                        if check_adjacent_symbol(data, k, h):
                            total += int(''.join(row[i:j]))
                            i = k
                            break

            else:
                on_number = False

    print("Part one: " + str(total))


data = load_data('input.txt')
part_one(data)