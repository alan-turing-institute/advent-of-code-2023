from copy import deepcopy
from time import time

import matplotlib.pyplot as plt
import numpy as np

start = time()

# ---------------------------------
# Parsing
# ---------------------------------


def parse_data(f):
    # grid is True if garden plot, False if rock
    grid = []
    for ir, row in enumerate(f.read().splitlines()):
        grid.append(list(row))
        for ic, col in enumerate(row):
            if col == "S":
                start = (ir, ic)
                grid[ir][ic] = "."

    return grid, start


def print_grid(grid, start):
    for ir, row in enumerate(grid):
        for ic, col in enumerate(row):
            if (ir, ic) == start:
                print("S", end="")
            else:
                print(col, end="")
        print()


with open(
    "/Users/jroberts/repos/advent-of-code-2023/day-21/python_jackr/input.txt"
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
        if 0 <= new_r < NROW and 0 <= new_c < NCOL and GRID[new_r][new_c] == ".":
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


# def adjacent(a, b):
#     delta = (a[0] - b[0], a[1] - b[1])
#     return delta in deltas


def get_neighbours_2(row, col):
    # grid now infinite
    neighbours = []
    for d in deltas:
        new_r = row + d[0]
        new_c = col + d[1]
        if GRID[new_r % NROW][new_c % NCOL] == ".":
            neighbours.append((new_r, new_c))

    # for n1 in neighbours[:-1]:
    #     for n2 in neighbours[1:]:
    #         if adjacent(n1, n2):
    #             grid[n1[0]][n1[1]] = "O"
    #             grid[n2[0]][n2[1]] = "O"
    #             break
    return neighbours


def part_2():
    t = time()
    reached = {START}
    print(f"{NROW=}, {NCOL=}")  # the grid is square
    # manual inspection of the input shows there's a clear line of sight (no rocks)
    # along the starting row or column. So it will take
    RADIUS = (NROW - 1) / 2
    # steps to reach the edge at the start. From then on every new map is entered from
    # an edge rather than the start, so it takes a full NROW (==NCOL) steps to reach
    # the next map. For the no. steps we're asked for that corresponds to:
    print("Will reach", (26501365 - RADIUS) / NROW, "maps horizontally and vertically")

    reached_history = []
    steps_history = []
    for n in range(600):
        if (n - RADIUS) % NROW == 0:
            steps_history.append(n)
            reached_history.append(len(reached))
        new = set()
        for r in reached:
            new.update(get_neighbours_2(r[0], r[1]))
        reached = new

        # cnt1 = 0
        # cnt2 = 0
        # for r in reached:
        #     if 0 <= r[0] < NROW and 0 <= r[1] < NCOL:
        #         cnt1 += 1
        #     if 0 <= r[0] < NROW and NCOL <= r[1] < 2 * NCOL:
        #         cnt2 += 1
    print(steps_history)
    print(reached_history)
    n_grids = list(range(len(steps_history)))
    coeffs = np.polyfit(n_grids, reached_history, 2)
    coeffs = np.round(coeffs).astype(int)
    print(coeffs)
    # plt.plot(n_grids, reached_history, "b")
    # plt.plot(n_grids, np.polyval(coeffs, n_grids), "r")
    # plt.show()

    # plt.scatter(*zip(*list(reached_history[-1])))
    # plt.show()
    # print(counts1)
    # print("---")
    # print(counts2)
    # print("---")
    # print(counts1.index(7307))
    # print(counts2.index(7307))
    # for r in reached:
    #     grid[r[0]][r[1]] = "O"
    for n in [
        0,
        1,
        2,
        3,
        4,
        202300,
    ]:  # [6, 10, 50, 100, 500, 1000, 26501365]:
        print(n, coeffs[0] * n**2 + coeffs[1] * n + coeffs[2])

    # ratio = 0
    # for r in GRID:
    #     for c in r:
    #         ratio += c == "."
    # ratio /= NROW * NCOL
    # n = 1000
    # print(ratio * ((n * 2 + 1) ** 2))

    print("Part 2:", f"(took {time() - t:.4f}s)")


if __name__ == "__main__":
    part_1()
    part_2()
