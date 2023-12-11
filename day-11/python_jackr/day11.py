from copy import deepcopy


def parse_data(file):
    return [list(row) for row in file.read().splitlines()]


def get_galaxies(universe):
    galaxies = []
    for row in range(len(universe)):
        for col in range(len(universe[0])):
            if universe[row][col] == "#":
                galaxies.append((row, col))
    return galaxies


def part_1(universe):
    # expand rows
    row = 0
    while row < len(universe):
        if all(space == "." for space in universe[row]):
            row += 1
            universe.insert(row, ["."] * len(universe[0]))
        row += 1

    # expand cols
    col = 0
    while col < len(universe[0]):
        column = [row[col] for row in universe]
        if all(space == "." for space in column):
            col += 1
            for row in universe:
                row.insert(col, ".")
        col += 1

    galaxies = get_galaxies(universe)

    distance = 0
    n_galaxies = len(galaxies)
    for g1 in range(n_galaxies - 1):
        for g2 in range(g1 + 1, n_galaxies):
            distance += abs(galaxies[g2][0] - galaxies[g1][0]) + abs(
                galaxies[g2][1] - galaxies[g1][1]
            )
    print("Part 1:", distance)


def part_2(universe):
    empty_rows = []
    for row in range(len(universe)):
        if all(space == "." for space in universe[row]):
            empty_rows.append(row)

    empty_cols = []
    for col in range(len(universe[0])):
        column = [row[col] for row in universe]
        if all(space == "." for space in column):
            empty_cols.append(col)

    galaxies = get_galaxies(universe)

    distance = 0
    n_galaxies = len(galaxies)
    for g1 in range(n_galaxies - 1):
        for g2 in range(g1 + 1, n_galaxies):
            min_row = min((galaxies[g2][0], galaxies[g1][0]))
            max_row = max((galaxies[g2][0], galaxies[g1][0]))
            min_col = min((galaxies[g2][1], galaxies[g1][1]))
            max_col = max((galaxies[g2][1], galaxies[g1][1]))
            distance += (max_row - min_row) + (max_col - min_col)

            for r in empty_rows:
                if min_row < r <= max_row:
                    distance += 999999
            for c in empty_cols:
                if min_col < c <= max_col:
                    distance += 999999

    print("Part 2:", distance)


if __name__ == "__main__":
    with open("input.txt") as f:
        universe = parse_data(f)
    part_1(deepcopy(universe))
    part_2(deepcopy(universe))
