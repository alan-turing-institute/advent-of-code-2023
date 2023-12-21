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
def get_neighbours_2(row, col):
    # grid now infinite, coordinates wrap round
    neighbours = []
    for d in deltas:
        new_r = row + d[0]
        new_c = col + d[1]
        if GRID[new_r % NROW][new_c % NCOL] == ".":
            neighbours.append((new_r, new_c))

    return neighbours


def part_2():
    """
    Not especially satisfied with this one, needed help from some visualisations I found
    online and I've not fully grasped why just extrapolating is enough, it feels like
    there should be some edge cases with the rocks where that may not be true.
    Then for a long time extrapolating wasn't working but it turned out to be an off
    by one error. That lost me a lot of time as I thought it was an error in the logic
    of extrapolating.
    """
    t = time()
    reached = {START}
    print(f"{NROW=}, {NCOL=}")  # the grid is square
    # manual inspection of the input shows there's a clear line of sight (no rocks)
    # along the starting row or column. So it will take
    RADIUS = (NROW - 1) / 2
    # steps to reach the edge at the start. From then on every new map is entered from
    # an edge rather than the start, so it takes a full NROW (==NCOL) steps to reach
    # the next map. For the no. steps we're asked for that corresponds to:
    SPAN = int((26501365 - RADIUS) / NROW)
    print("Will reach", SPAN, "maps horizontally and vertically")

    # check how many cells we could reach after the first few hundred steps, and
    # log the ones that correspond to reaching the edges of maps.
    reached_history = []
    steps_history = []
    for n in range(600):  # technically only needs to run up to RADIUS + 2*NROW + 1
        if (n - RADIUS) % NROW == 0:
            steps_history.append(n)
            reached_history.append(len(reached))
        new = set()
        for r in reached:
            new.update(get_neighbours_2(r[0], r[1]))
        reached = new

    # Then fit a quadratic to it and extrapolate. Quadratic as the max possible area
    # grows as a square, and extrapolating works as the map cells make a repeating
    # pattern.
    # I allowed myself the luxury of numpy's quadratic fitting here...
    print(f"{steps_history=}")
    print(f"{reached_history=}")
    n_grids = list(range(len(steps_history)))
    coeffs, resid, *_ = np.polyfit(n_grids, reached_history, 2, full=True)
    print(f"{resid=}")
    print(f"{coeffs=}")
    coeffs = np.round(coeffs).astype(int)  # to avoid any floating point errors

    # # sanity check plot to show it is quadratic
    # plt.plot(n_grids, reached_history, "o")
    # plt.plot(n_grids, np.polyval(coeffs, n_grids))
    # plt.show()

    print(
        "Part 2:",
        coeffs[0] * SPAN**2 + coeffs[1] * SPAN + coeffs[2],
        f"(took {time() - t:.4f}s)",
    )


if __name__ == "__main__":
    part_1()
    part_2()
