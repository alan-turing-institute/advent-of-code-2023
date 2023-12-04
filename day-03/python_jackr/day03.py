def parse_data(file):
    return [line.strip() for line in file.readlines()]


# ----------------------
# PART 1
# ----------------------
def has_adjacent_symbol(row, column, data):
    for dx in [-1, 0, 1]:
        if (column + dx < 0) or (column + dx >= len(data[0])):
            continue
        for dy in [-1, 0, 1]:
            if (row + dy < 0) or (row + dy >= len(data)):
                continue
            if is_symbol(data[row + dy][column + dx]):
                return True
    return False


def is_symbol(string):
    return (not string.isdigit()) and (string != ".")


def part_1(data):
    row = 0
    include_numbers = []
    while row < len(data):
        column = 0
        while column < len(data[row]):
            if data[row][column].isdigit():
                include = has_adjacent_symbol(row, column, data)
                number = data[row][column]

                # keep scanning columns to construct the full number and check for
                # adjacent symbols
                column += 1
                while column < len(data[row]) and data[row][column].isdigit():
                    number += data[row][column]
                    include = include or has_adjacent_symbol(row, column, data)
                    column += 1
                if include:
                    include_numbers.append(int(number))

            column += 1
        row += 1

    print("Part 1:", sum(include_numbers))


# ----------------------
# PART 2
# ----------------------
def expand_number(row, column, data):
    """
    Return full number at row/column, where row/column may point to the middle of the
    number, and the column where the number ends
    """
    start_col = column
    while start_col >= 0 and data[row][start_col].isdigit():
        start_col -= 1
    end_col = column
    while end_col < len(data[0]) and data[row][end_col].isdigit():
        end_col += 1
    return int(data[row][(start_col + 1) : end_col]), end_col


def get_adjacent_numbers(row, column, data):
    numbers = []
    for dy in [-1, 0, 1]:
        if (row + dy < 0) or (row + dy >= len(data)):
            continue
        dx = -1
        while dx <= 1:
            if (column + dx < 0) or (column + dx >= len(data[0])):
                continue
            if data[row + dy][column + dx].isdigit():
                num, end_column = expand_number(row + dy, column + dx, data)
                numbers.append(num)
                dx = end_column - column
            else:
                dx += 1
    return numbers


def part_2(data):
    total = 0
    for row in range(len(data)):
        for column in range(len(data[0])):
            if data[row][column] == "*":
                numbers = get_adjacent_numbers(row, column, data)
                if len(numbers) == 2:
                    total += numbers[0] * numbers[1]

    print("Part 2:", total)


if __name__ == "__main__":
    with open("input.txt") as f:
        data = parse_data(f)

    part_1(data)
    part_2(data)
