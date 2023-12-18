import heapq
import sys
from collections import defaultdict
from dataclasses import dataclass
from time import time


def parse_data(file):
    lines = file.read().splitlines()
    return [[int(col) for col in row] for row in lines]


directions = {"L": (0, -1), "R": (0, 1), "U": (-1, 0), "D": (1, 0)}
opposites = {"L": "R", "R": "L", "U": "D", "D": "U"}


@dataclass(frozen=True)
class Node:
    row: int
    col: int
    direct: str  # direction entered
    steps: int  # no. steps in this direction so far

    def __lt__(self, other):
        # just needed in case there are ties in priority on the heapq, in which case
        # it will try to sort the nodes. Just pick the one closest to bottom right in
        # that case.
        return self.row >= other.row and self.col > other.col


def get_neighbours(node, nrow, ncol, min_steps, max_steps):
    neighbours = []
    for d in directions.keys():
        # don't turn 180
        if d == opposites[node.direct]:
            continue
        # don't exceed grid bounds
        delta = directions[d]
        new_pos = (node.row + delta[0], node.col + delta[1])
        if not 0 <= new_pos[0] < nrow:
            continue
        if not 0 <= new_pos[1] < ncol:
            continue
        # consec steps limits
        if node.steps == max_steps and d == node.direct:
            continue
        if node.steps < min_steps and d != node.direct:
            continue

        if d == node.direct:
            # take extra step in this direction
            neighbours.append(Node(new_pos[0], new_pos[1], d, node.steps + 1))
        else:
            # first step in this direction
            neighbours.append(Node(new_pos[0], new_pos[1], d, 1))
    return neighbours


def is_end(node, nrow, ncol, min_steps, max_steps):
    return (
        (node.row == nrow - 1)
        and (node.col == ncol - 1)
        and (node.steps >= min_steps)
        and (node.steps <= max_steps)
    )


def dijkstra(grid, min_steps, max_steps):
    # set up some vars
    dist = defaultdict(lambda: sys.maxsize)
    nrow = len(grid)
    ncol = len(grid[0])

    # one entries for each possible starting direction
    queue = [(0, Node(0, 0, "R", 0)), (0, Node(0, 0, "D", 0))]
    for q in queue:
        dist[q[1]] = 0

    while queue:
        # min distance out of nodes in queue
        _, node = heapq.heappop(queue)

        if is_end(node, nrow, ncol, min_steps, max_steps):
            # finished
            break

        for new in get_neighbours(node, nrow, ncol, min_steps, max_steps):
            # if new in queue:  # DEBUG
            dist_to_new = dist[node] + grid[new.row][new.col]
            if dist_to_new < dist[new]:
                dist[new] = dist_to_new

                heapq.heappush(queue, (dist[new], new))

    return dist[node]


def part_1(grid):
    t = time()
    print("Part 1:", dijkstra(grid, 1, 3), f"(took {time() - t:.4f}s)")


def part_2(grid):
    t = time()
    print("Part 2:", dijkstra(grid, 4, 10), f"(took {time() - t:.4f}s)")


if __name__ == "__main__":
    with open("input.txt") as f:
        grid = parse_data(f)
    part_1(grid)
    part_2(grid)
