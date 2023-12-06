import math
import re


def parse_data(file):
    sections = file.read().splitlines()
    times = re.findall(r"\d+", sections[0])
    distances = re.findall(r"\d+", sections[1])
    return [int(t) for t in times], [int(d) for d in distances]


def solve(T, d):
    # T: race duration
    # d: distance to beat
    # t1: min button press time
    # t2: max button press time
    t1 = (T - math.sqrt(T**2 - 4 * d)) / 2
    t2 = (T + math.sqrt(T**2 - 4 * d)) / 2
    # no. ways to beat record
    return math.floor(t2) - math.ceil(t1) + 1


def part_1(times, distances):
    no_ways = [solve(T, d) for T, d in zip(times, distances)]
    print("Part 1:", math.prod(no_ways))


def part_2(times, distances):
    T = int("".join([str(t) for t in times]))
    d = int("".join([str(d) for d in distances]))
    print("Part 2:", solve(T, d))


if __name__ == "__main__":
    with open("input.txt") as f:
        times, distances = parse_data(f)
    part_1(times, distances)
    part_2(times, distances)
