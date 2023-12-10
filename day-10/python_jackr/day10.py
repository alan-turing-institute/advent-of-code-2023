from collections import defaultdict


def parse_data(file):
    lines = file.read().splitlines()
    grid = []
    for line in lines:
        grid.append([cell for cell in line])
    return grid


# directions (row change, column change)
NORTH = (-1, 0)
SOUTH = (1, 0)
EAST = (0, 1)
WEST = (0, -1)

# directions you can move from a pipe of this type
PIPES_OUT = {
    "|": (NORTH, SOUTH),
    "-": (EAST, WEST),
    "L": (NORTH, EAST),
    "J": (NORTH, WEST),
    "7": (SOUTH, WEST),
    "F": (SOUTH, EAST),
}
# directions from which you can enter a pipe of this type (opposite of above)
PIPES_IN = {
    "|": (SOUTH, NORTH),
    "-": (WEST, EAST),
    "L": (SOUTH, WEST),
    "J": (SOUTH, EAST),
    "7": (NORTH, EAST),
    "F": (NORTH, WEST),
}

GROUND = "."
START = "S"


def get_start(grid):
    for row in range(len(grid)):
        for col in range(len(grid[0])):
            if grid[row][col] == START:
                return (row, col)


def get_next(node, previous, grid):
    directions = PIPES_OUT[grid[node[0]][node[1]]]
    for d in directions:
        new_node = (node[0] + d[0], node[1] + d[1])
        new_type = grid[new_node[0]][new_node[1]]
        if new_type == GROUND:
            continue
        if d in PIPES_IN[new_type] and new_node != previous:
            return new_node
    return []  # dead end


def find_loop(start, grid):
    # kinda breadth-first search
    visited = [[False for _ in range(len(grid[0]))] for _ in range(len(grid))]
    loop = []
    queue = [(start, [])]
    is_start = True
    while len(queue) > 0:
        now, previous = queue.pop(0)
        # hacky additional is_start check to avoid instantly returning as the start
        # node = goal node
        if (not is_start) and now == start:
            return loop  # this is the loop
        else:
            loop.append(now)
            is_start = False
        # find where we're going next
        next = get_next(now, previous, grid)
        if next:
            visited[next[0]][next[1]] = True
            queue.append((next, now))
    return []  # not a loop


def part_1(grid):
    start = get_start(grid)
    for start_type in PIPES_OUT.keys():
        grid[start[0]][start[1]] = start_type
        loop = find_loop(start, grid)
        if len(loop) > 0:
            break  # found the loop
    print("Part 1:", len(loop) / 2)
    return loop


def part_2(loop, grid):
    on_loop = [[False for _ in range(len(grid[0]))] for _ in range(len(grid))]
    for node in loop:
        on_loop[node[0]][node[1]] = True

    n_inside = 0
    for row in range(len(on_loop)):
        inside = False
        for col in range(len(on_loop[0])):
            if on_loop[row][col]:
                if grid[row][col] in ["|", "L", "J"]:
                    # each time we cross a vertical line (when scanning horizontally) in
                    # the loop we move from outside of it to inside (or vice-versa).
                    # Need to include either ("L" and "J") or ("F" and "7") as vertical
                    # lines too, but not both, depending on how we're treating the
                    # corners (which row the horizontal edge of the corner belongs to)
                    # |  *  |     |--x--|
                    # |     |     |     |
                    # |__x__|  or |  *  |
                    # I don't think I've understood this well enough to explain it but
                    # it worked and it's late...
                    inside = not inside
            else:
                n_inside += inside

    print("Part 2:", n_inside)


if __name__ == "__main__":
    with open("input.txt") as f:
        grid = parse_data(f)
    loop = part_1(grid)
    part_2(loop, grid)
