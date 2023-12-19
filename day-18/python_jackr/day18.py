from time import time

# ======== PART 1 =========


def parse_data_1(file):
    lines = file.read().splitlines()
    # plan = []
    visited = [(0, 0)]  # list of all visited points
    directions = {"L": (0, -1), "R": (0, 1), "U": (-1, 0), "D": (1, 0)}

    for line in lines:
        direct, length, _ = line.split()
        delta = directions[direct]
        for _ in range(int(length)):
            last = visited[-1]
            visited.append((last[0] + delta[0], last[1] + delta[1]))

    return visited


def build_grid(visited):
    # list of visited points to grid encompassing all visited points
    # minus/plus 1s to add border around grid
    row_span = (min(p[0] for p in visited) - 1, max(p[0] for p in visited) + 1)
    col_span = (min(p[1] for p in visited) - 1, max(p[1] for p in visited) + 1)
    grid = [
        [False for _ in range(col_span[1] - col_span[0] + 1)]
        for _ in range(row_span[1] - row_span[0] + 1)
    ]
    for p in visited:
        grid[p[0] - row_span[0]][p[1] - col_span[0]] = True
    return grid


def flood_fill(dug):
    # start outside the dug path and keep searching for non-dug neighbours
    nrows = len(dug)
    ncols = len(dug[0])
    filled = [[False for _ in range(ncols)] for _ in range(nrows)]
    visited = [[False for _ in range(ncols)] for _ in range(nrows)]
    queue = [(0, 0)]
    while queue:
        pos = queue.pop()
        visited[pos[0]][pos[1]] = True
        filled[pos[0]][pos[1]] = True
        for new_pos in [
            (pos[0] + 1, pos[1]),
            (pos[0] - 1, pos[1]),
            (pos[0], pos[1] + 1),
            (pos[0], pos[1] - 1),
        ]:
            if not 0 <= new_pos[0] < nrows:
                continue
            if not 0 <= new_pos[1] < ncols:
                continue
            if not visited[new_pos[0]][new_pos[1]] and not dug[new_pos[0]][new_pos[1]]:
                queue.append(new_pos)

    # area = no. of points we didn't fill
    area = 0
    for row in filled:
        for col in row:
            if not col:
                area += 1
    return area


def part_1(path):
    t = time()
    with open(path) as f:
        visited = parse_data_1(f)
    grid = build_grid(visited)
    print("Part 1:", flood_fill(grid), f"(took {time() - t:.4f}s)")


# ======== PART 2 =========
# only with A LOT of searching around and looking for hints... I had no idea about
# Shoelace or Pick's theorem. Like on day 10 figuring out how to treat the edges
# properly wreaks havoc.


def parse_data_2(file):
    lines = file.read().splitlines()
    edges = [(0, 0)]  # list of corners (instruction points)
    # directions are now ints
    directions = {"2": (0, -1), "0": (0, 1), "3": (-1, 0), "1": (1, 0)}
    perimeter = 0  # no. of blocks on perimeter
    for line in lines:
        _, _, color = line.split()
        direct = color[-2]  # direction is last digit (excluding bracket)
        # step size is remaining hexadecimal (brackets, # and last digit removed)
        length = int(color[2:-2], 16)
        perimeter += length
        # map distance to coordinates delta and multiply dy instruction length
        step = (
            directions[direct][0] * length,
            directions[direct][1] * length,
        )
        # new edge location is previous + step
        edges.append((edges[-1][0] + step[0], edges[-1][1] + step[1]))
    return edges[:-1], perimeter  # remove last so (0, 0) not included twice


def shoelace(points):
    # https://en.wikipedia.org/wiki/Shoelace_formula
    # this gets the area INSIDE the points, where points must be in the order needed
    # to draw the shape (i.e. draw line from point to point)
    # computed by adding/subtracting trapezoidal areas
    N = len(points)
    area = 0
    for i in range(N):
        xi = points[i][0]
        yi = points[i][1]

        j = (i + 1) % N  # last point loops back to start
        xj = points[j][0]
        yj = points[j][1]

        area += (yi + yj) * (xi - xj)

    return abs(area / 2)


def get_area(edge_coords, perimeter):
    shoelace_area = shoelace(edge_coords)
    # the shoelace area computes the area as if the coordinates are the mid-point of
    # a unit square, but we want the area to the edge of the dug squares
    # - perimeter cells on straight edges: add 0.5 units to the area
    # - anti-clockwise corners add 0.25 units to the area
    # - clockwise corners add 0.75 units to the area
    # - a pair of (CW, anti-CW) corners adds an average of 0.5 to the area each
    # - a closed loop has 4 more clockwise corners compared to anti-CW (to close the loop)
    # all together, the perimeter contributes 0.5 per cell + (4 * 0.75-0.5) for the extra area in the 4 additional CW corners
    # This might be Pick's theorem: https://en.wikipedia.org/wiki/Pick%27s_theorem
    return int(shoelace_area + perimeter // 2 + 1)


def part_2(path):
    t = time()
    with open(path) as f:
        edges, perimeter = parse_data_2(f)
    print("Part 2:", get_area(edges, perimeter), f"(took {time() - t:.4f}s)")


if __name__ == "__main__":
    path = "input.txt"
    part_1(path)
    part_2(path)
