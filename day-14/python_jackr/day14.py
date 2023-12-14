from time import time


def parse_data(file):
    platform = file.read().splitlines()
    return [list(row) for row in platform]


def platform_str(platform):
    return "\n".join(["".join(r) for r in platform])


def tilt_north(platform):
    for col in range(len(platform[0])):
        for row in range(1, len(platform)):
            if platform[row][col] == "O":
                # replace current item with empty space
                platform[row][col] = "."
                # find topmost row we can move this O to
                move_row = row
                while move_row >= 0 and platform[move_row][col] == ".":
                    move_row -= 1
                # move O to topmost available row
                platform[move_row + 1][col] = "O"

    return platform


def tilt_west(platform):
    for col in range(1, len(platform[0])):
        for row in range(len(platform)):
            if platform[row][col] == "O":
                # replace current item with empty space
                platform[row][col] = "."
                # find leftmost col we can move this O to
                move_col = col
                while move_col >= 0 and platform[row][move_col] == ".":
                    move_col -= 1
                # move O to leftmost available row
                platform[row][move_col + 1] = "O"
    return platform


def tilt_south(platform):
    for col in range(len(platform[0])):
        for row in range(len(platform) - 2, -1, -1):
            if platform[row][col] == "O":
                # replace current item with empty space
                platform[row][col] = "."
                # find bottom-most row we can move this O to
                move_row = row
                while move_row < len(platform) and platform[move_row][col] == ".":
                    move_row += 1
                # move O to bottom-most available row
                platform[move_row - 1][col] = "O"

    return platform


def tilt_east(platform):
    for col in range(len(platform[0]) - 2, -1, -1):
        for row in range(len(platform)):
            if platform[row][col] == "O":
                # replace current item with empty space
                platform[row][col] = "."
                # find right-most row we can move this O to
                move_col = col
                while move_col < len(platform[0]) and platform[row][move_col] == ".":
                    move_col += 1
                # move O to right-most available row
                platform[row][move_col - 1] = "O"

    return platform


def cycle(platform):
    platform = tilt_north(platform)
    platform = tilt_west(platform)
    platform = tilt_south(platform)
    platform = tilt_east(platform)
    return platform


def get_load(platform):
    nrows = len(platform)
    load = 0
    for idx, row in enumerate(platform):
        for symbol in row:
            if symbol == "O":
                load += nrows - idx
    return load


def part_1(platform):
    t = time()
    platform = tilt_north(platform)
    print("Part 1:", get_load(platform), f"(took {time() - t:.4f}s)")


def part_2(platform):
    t = time()

    # run cycles until we notice a repeating pattern
    N = 1000000000
    n = 0
    states = {}  # str representation of platform: no. cycles when first seen
    while n < N:
        platform = cycle(platform)
        n += 1
        s = platform_str(platform)
        if s in states:
            break
        states[s] = n

    # determine how many remaining cycles to run to reach N after repeating the pattern
    # as much as possible
    start = states[s]
    length = n - start
    remaining = N - n
    remainder = remaining % length
    for _ in range(remainder):
        platform = cycle(platform)

    print("Part 2:", get_load(platform), f"(took {time() - t:.4f}s)")


if __name__ == "__main__":
    with open("input.txt") as f:
        platform = parse_data(f)

    part_1(platform)
    part_2(platform)
