import math
import re


def parse_data(file):
    lines = file.read().splitlines()
    instructions = lines[0]
    network = {}
    for line in lines[2:]:
        nodes = re.findall(r"[\w\d]+", line)
        network[nodes[0]] = (nodes[1], nodes[2])
    return instructions, network


def step(node, n_steps, instructions, network):
    direction = instructions[n_steps % len(instructions)]
    if direction == "L":
        return network[node][0], n_steps + 1
    return network[node][1], n_steps + 1


def part_1(instructions, network):
    node = "AAA"
    n_steps = 0
    while node != "ZZZ":
        node, n_steps = step(node, n_steps, instructions, network)

    print("Part 1:", n_steps)


def part_2(instructions, network):
    starts = [n for n in network.keys() if n.endswith("A")]
    n_steps = [0] * len(starts)  # how many steps to a Z node for each start node
    for idx, node in enumerate(starts):
        # each path from a start node only visits one node that ends with Z (even if
        # you continue traversing the network after reaching a Z node). If that
        # wasn't true we'd need to check for more possible end conditions.
        while not node.endswith("Z"):
            node, n_steps[idx] = step(node, n_steps[idx], instructions, network)

    # earliest n_steps all nodes end with Z (lowest common multiple of steps taken to
    # reach Z for each start node)
    print("Part 2:", math.lcm(*n_steps))


if __name__ == "__main__":
    with open("input.txt") as f:
        instructions, network = parse_data(f)
    part_1(instructions, network)
    part_2(instructions, network)
