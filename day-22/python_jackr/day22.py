from copy import deepcopy
from dataclasses import dataclass
from time import time

start = time()


# ---------------------------------
# Parsing
# ---------------------------------
@dataclass
class Point:
    x: int
    y: int
    z: int


def parse_data(f):
    # bricks are 2 points (bottom left to top right)
    bricks = []
    for line in f.read().splitlines():
        left, right = line.split("~")
        left = Point(*[int(i) for i in left.split(",")])
        right = Point(*[int(i) for i in right.split(",")])
        bricks.append([left, right])

    # sort so lowest brick (first z coordinate) comes first
    return list(sorted(bricks, key=lambda b: b[0].z))


# ---------------------------------
# Part 1
# ---------------------------------
def drop(bricks, return_if_dropped=False, count_dropped=False):
    # array to store height of floor (z) at each x, y coordinate
    max_x = max(b[1].x for b in bricks)
    max_y = max(b[1].y for b in bricks)
    floor = [[0 for _ in range(max_y + 1)] for _ in range(max_x + 1)]
    count = 0
    for b in bricks:
        xs = range(b[0].x, b[1].x + 1)
        ys = range(b[0].y, b[1].y + 1)
        zs = range(b[0].z, b[1].z + 1)
        if len(zs) > 1:  # vertically oriented brick
            x = xs[0]
            y = ys[0]
            delta = b[0].z - (floor[x][y] + 1)
            floor[x][y] += len(zs)
            if return_if_dropped and delta > 0:
                return True
            if count_dropped and delta > 0:
                count += 1
            b[0].z -= delta
            b[1].z -= delta
        else:
            z = zs[0]
            new_z = max(floor[x][y] for x in xs for y in ys) + 1  # collision height
            for x in xs:
                for y in ys:
                    floor[x][y] = new_z
            delta = z - new_z
            if return_if_dropped and delta > 0:
                return True
            if count_dropped and delta > 0:
                count += 1
            b[0].z -= delta
            b[1].z -= delta

    if return_if_dropped:
        return False
    if count_dropped:
        return count


def part_1(bricks):
    t = time()
    drop(bricks)

    support_count = 0
    for idx in range(len(bricks)):
        without_b = deepcopy(bricks)
        without_b.pop(idx)
        # was supporting other bricks if removing it causes another brick to fall
        support_count += drop(without_b, return_if_dropped=True)

    print("Part 1:", len(bricks) - support_count, f"(took {time() - t:.4f}s)")


# ---------------------------------
# Part 2
# ---------------------------------
def part_2(bricks):
    t = time()
    drop(bricks)

    support_count = []
    for idx in range(len(bricks)):
        without_b = deepcopy(bricks)
        without_b.pop(idx)
        # was supporting other bricks if removing it causes another brick to fall
        support_count.append(drop(without_b, count_dropped=True))

    print("Part 2:", sum(support_count), f"(took {time() - t:.4f}s)")


if __name__ == "__main__":
    with open("input.txt") as f:
        bricks = parse_data(f)

    part_1(bricks)
    part_2(bricks)
    print(f"(overall {time() - start:.4f}s)")
