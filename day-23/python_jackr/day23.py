import heapq
import sys
from collections import defaultdict
from copy import deepcopy
from dataclasses import dataclass
from time import time

start = time()


@dataclass  # (frozen=True)
class Node:
    row: int
    col: int
    kind: str
    steps: int = 1

    def __post_init__(self):
        self.children: list[Node] = []
        self.distance: int = 0  # distance to end (to be filled later)
        self.visited: bool = False

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


def build_graph():
    queue = [GRID[0][1]]
    graph = []
    while queue:
        current = queue.pop()
        if current not in graph:
            graph.append(current)
            neighbours = get_neighbours(current, graph, check_slopes=False)
            current.children += neighbours
            queue += neighbours

    print(len(graph))
    print()

    while any(len(n.children) == 1 for n in graph):
        for v in graph:
            if len(v.children) == 1:
                for c in v.children:
                    c.steps += v.steps
                graph.remove(v)
                break

    return graph


def backfill_distance():
    queue = [GRID[NROW - 1][NCOL - 2]]
    while queue:
        current = queue.pop()
        for p in current.parents:
            p.distance = current.distance + 1
            queue.append(p)
    return GRID[0][1].distance


def dijkstra():
    queue = [GRID[NROW - 1][NCOL - 2]]

    while queue:
        # node = queue.pop()
        node = min(queue, key=lambda n: n.distance)
        queue.remove(node)

        # if node.row == 0 and node.col == 1:
        #     # finished
        #     break

        for new in node.parents:
            dist_to_new = node.distance - 1
            if dist_to_new < new.distance:
                new.distance = dist_to_new
                queue.append(new)

    return GRID[0][1].distance  # node.distance


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
    paths = [(GRID[0][1], set())]
    count = 0
    while paths:
        current, previous = paths.pop()
        previous = previous.copy()
        previous.add(current)
        count += 1
        if current.row == NROW - 1 and current.col == NCOL - 2:
            # this is the target cell
            found.append(previous)
            # print_grid(previous)
            print(
                f"{len(found)} found, {len(paths)} in progress, longest {max(len(f) for f in found) - 1}, {count} iterations"
            )
        else:
            neighbours = get_neighbours(current, previous, check_slopes)
            for n in neighbours:
                paths.append((n, previous))

    return found


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
    # part_1()
    # part_2()
    print(build_graph())
    # print(backfill_distance())  # dijkstra()
    print(f"(overall {time() - start:.4f}s)")
