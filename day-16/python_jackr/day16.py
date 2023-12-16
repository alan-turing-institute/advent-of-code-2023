from time import time


def parse_data(file):
    return file.read().splitlines()


def print_visited(visited):
    for r in visited:
        for c in r:
            if c:
                print("#", end="")
            else:
                print(".", end="")
        print()


def get_energy(entry, grid):
    nrow = len(grid)
    ncol = len(grid[0])
    beams = [entry]  # list of (row, col, row velocity, col velocity)
    energised = [[False for _ in range(ncol)] for _ in range(nrow)]
    states = set()
    while beams:
        new_beams = []
        for beam in beams:
            row, col, vrow, vcol = beam
            row = row + vrow
            col = col + vcol
            if row < 0 or col < 0 or row >= nrow or col >= ncol:
                # left the grid
                continue
            if (row, col, vrow, vcol) in states:
                # been here before so we know all the states that will be visited after
                # this one
                continue
            states.add((row, col, vrow, vcol))
            energised[row][col] = True

            kind = grid[row][col]
            if kind == "/":
                # reflect (swaps row and column velocity, with opposite sign)
                new_beams.append((row, col, -vcol, -vrow))
            elif kind == "\\":
                # reflect (swaps row and column velocity)
                new_beams.append((row, col, vcol, vrow))
            elif kind == "|" and vrow == 0:
                # split
                new_beams.append((row, col, 1, 0))
                new_beams.append((row, col, -1, 0))
            elif kind == "-" and vcol == 0:
                # split
                new_beams.append((row, col, 0, 1))
                new_beams.append((row, col, 0, -1))
            else:
                # continue as if in free space
                new_beams.append((row, col, vrow, vcol))

        beams = new_beams

    return sum(col for row in energised for col in row)


def part_1(grid):
    t = time()
    print(
        "Part 1:",
        get_energy((0, -1, 0, 1), grid),  # beam enters from top left, moving right
        f"(took {time() - t:.4f}s)",
    )


def part_2(grid):
    t = time()
    entries = []
    nrow = len(grid)
    ncol = len(grid[0])
    for row in range(nrow):
        entries.append((row, -1, 0, 1))
        entries.append((row, ncol, 0, -1))
    for col in range(ncol):
        entries.append((-1, col, 1, 0))
        entries.append((ncol, col, -1, 0))

    print(
        "Part 2:",
        max(get_energy(e, grid) for e in entries),
        f"(took {time() - t:.4f}s)",
    )


if __name__ == "__main__":
    with open("input.txt") as f:
        grid = parse_data(f)
    part_1(grid)
    part_2(grid)
