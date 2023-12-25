from time import time

import networkx as nx

start = time()


def parse_data(file):
    lines = file.read().splitlines()
    nodes = set()
    edges = []
    for line in lines:
        left, right = line.split(": ")
        nodes.add(left)
        right = right.split(" ")
        nodes.update(right)
        for r in right:
            edges.append((left, r))
    return nodes, edges


with open("input.txt") as f:
    NODES, EDGES = parse_data(f)


def part_1():
    t = time()

    G = nx.Graph()
    for n in NODES:
        G.add_node(n)
    for e in EDGES:
        G.add_edge(e[0], e[1], capacity=1.0)

    edges_to_cut = nx.minimum_edge_cut(G)
    print("Found 3 edges to cut", len(edges_to_cut) == 3)

    for cut in edges_to_cut:
        for e in EDGES:
            if cut[0] in e and cut[1] in e:
                EDGES.remove(e)
                break

    G = nx.Graph()
    for n in NODES:
        G.add_node(n)
    for e in EDGES:
        G.add_edge(e[0], e[1], capacity=1.0)

    parts = list(nx.connected_components(G))
    print("Has 2 parts now:", len(parts) == 2)

    print("Part 1:", len(parts[0]) * len(parts[1]), f"(took {time() - t:.4f}s)")


if __name__ == "__main__":
    part_1()
    print(f"(overall {time() - start:.4f}s)")
