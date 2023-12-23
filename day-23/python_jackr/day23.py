import heapq
import sys
from collections import defaultdict
from copy import deepcopy
from dataclasses import dataclass
from time import time

start = time()


@dataclass(frozen=True)
class Node:
    row: int
    col: int
    kind: str

    def __lt__(self, other):
        # just needed in case there are ties in priority on the heapq, in which case
        # it will try to sort the nodes. Just pick the one furthest from bottom right in
        # that case.
        return ((NROW - 1 - self.row) + (NCOL - 1 - self.col)) > (
            (NROW - 1 - other.row) + (NCOL - 1 - other.col)
        )


def parse_data(file):
    lines = file.read().splitlines()
    return [
        [Node(r, c, kind) for c, kind in enumerate(row)] for r, row in enumerate(lines)
    ]


directions = {"L": (0, -1), "R": (0, 1), "U": (-1, 0), "D": (1, 0)}

with open(
    "/Users/jroberts/repos/advent-of-code-2023/day-23/python_jackr/test_input.txt"
) as f:
    GRID = parse_data(f)
NROW = len(GRID)
NCOL = len(GRID[0])


def get_neighbours(node, path, check_slopes=True):
    if check_slopes:
        # fall down slopes
        if node.kind == ">":
            if GRID[node.row][node.col + 1] in path:
                # slope would make us backtrack so no neighbours
                return []
            return [GRID[node.row][node.col + 1]]
        if node.kind == "v":
            if GRID[node.row + 1][node.col] in path:
                # slope would make us backtrack so no neighbours
                return []
            return [GRID[node.row + 1][node.col]]

    # non-slope
    neighbours = []
    for d in directions.keys():
        # don't exceed grid bounds
        delta = directions[d]
        new_pos = (node.row + delta[0], node.col + delta[1])
        if not 0 <= new_pos[0] < NROW:
            continue
        if not 0 <= new_pos[1] < NCOL:
            continue
        new_node = GRID[new_pos[0]][new_pos[1]]
        # don't walk in the forest
        if new_node.kind == "#":
            continue
        # don't move to already visisted nodes
        if new_node in path:
            continue
        neighbours.append(new_node)

    return neighbours


def find_paths(check_slopes=True):
    # continue path until next fork
    found = []
    paths = [(GRID[0][1],)]
    count = 0
    while paths:
        p = paths.pop()
        count += 1
        if p[-1].row == NROW - 1 and p[-1].col == NCOL - 2:
            # this is the target cell
            found.append(p)
            # print_grid(p)
            # print(
            #     f"\n---{len(found)} found, {len(paths)} in progress, {count} iterations---\n"
            # )

        else:
            neighbours = get_neighbours(p[-1], p, check_slopes)
            for n in neighbours:
                paths.append(p + (n,))

    return found


def dijkstra():
    # set up some vars
    dist = defaultdict(lambda: sys.maxsize)

    # one entries for each possible starting direction
    queue = [(0, GRID[0][1], (GRID[0][1],))]
    heapq.heapify(queue)
    for q in queue:
        dist[q[1]] = 0

    while queue:
        # min distance out of nodes in queue
        _, node, path = heapq.heappop(queue)

        if node.row == NROW - 1 and node.col == NCOL - 2:
            # finished
            break

        for new in get_neighbours(node, path):
            dist_to_new = dist[node] + 1
            if dist_to_new < dist[new]:
                dist[new] = dist_to_new

                # minus so max distance comes first
                heapq.heappush(queue, (dist[new], new, path + (new,)))

    return dist[node]


def print_grid(path):
    pgrid = deepcopy(GRID)
    for p in path:
        pgrid[p.row][p.col] = Node(p.row, p.col, "O")
    for r in pgrid:
        for c in r:
            print(c.kind, end="")
        print()


def part_1():
    t = time()
    paths = find_paths(check_slopes=True)
    longest = max(len(p) for p in paths) - 1  # -1 as paths include start

    print("Part 1:", longest, f"(took {time() - t:.4f}s)")


def part_2():
    t = time()
    paths = find_paths(check_slopes=False)
    longest = max(len(p) for p in paths) - 1  # -1 as paths include start
    print("Part 2:", longest, f"(took {time() - t:.4f}s)")


if __name__ == "__main__":
    part_1()
    part_2()
    print(f"(overall {time() - start:.4f}s)")
