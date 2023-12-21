from time import time

start = time()

# ---------------------------------
# Parsing
# ---------------------------------


def parse_data(f):
    # grid is True if garden plot, False if rock
    grid = []
    for ir, row in enumerate(f.read().splitlines()):
        grid.append([])
        for ic, col in enumerate(row):
            if col == "#":
                grid[-1].append(False)
            elif col == "S":
                start = (ir, ic)
                grid[-1].append(True)
            else:
                grid[-1].append(True)

    return grid, start


def print_grid(grid, start):
    for ir, row in enumerate(grid):
        for ic, col in enumerate(row):
            if (ir, ic) == start:
                print("S", end="")
            elif col:
                print(".", end="")
            else:
                print("#", end="")
        print()


with open(
    "/Users/jroberts/repos/advent-of-code-2023/day-21/python_jackr/test_input.txt"
) as f:
    GRID, START = parse_data(f)
NROW = len(GRID)
NCOL = len(GRID[0])

# ---------------------------------
# Part 1
# ---------------------------------

deltas = ((1, 0), (0, 1), (-1, 0), (0, -1))


def get_neighbours_1(row, col):
    neighbours = []
    for d in deltas:
        new_r = row + d[0]
        new_c = col + d[1]
        if 0 <= new_r < NROW and 0 <= new_c < NCOL and GRID[new_r][new_c]:
            neighbours.append((new_r, new_c))
    return neighbours


def part_1():
    t = time()
    reached = {START}
    for _ in range(64):
        new = set()
        for r in reached:
            new.update(get_neighbours_1(r[0], r[1]))
        reached = new

    print("Part 1:", len(reached), f"(took {time() - t:.4f}s)")


# ---------------------------------
# Part 2
# ---------------------------------


def get_neighbours_2(row, col):
    # grid now infinite
    neighbours = []
    for d in deltas:
        new_r = row + d[0]
        new_c = col + d[1]
        if GRID[new_r % NROW][new_c % NCOL]:
            neighbours.append((new_r, new_c))
    return neighbours


def part_2():
    t = time()
    reached = {START}
    for _ in range(5000):
        new = set()
        for r in reached:
            new.update(get_neighbours_2(r[0], r[1]))
        reached = new

    print("Part 2:", len(reached), f"(took {time() - t:.4f}s)")


if __name__ == "__main__":
    part_1()
    part_2()
