from collections import OrderedDict, defaultdict
from time import time


def parse_data(file):
    return file.read().strip().split(",")


def hash(string):
    value = 0
    for s in string:
        value += ord(s)
        value *= 17
        value %= 256
    return value


def part_1(sequence):
    t = time()
    print("Part 1:", sum(hash(s) for s in sequence), f"(took {time() - t:.4f}s)")


def part_2(sequence):
    t = time()
    # place lenses
    boxes = defaultdict(OrderedDict)
    for s in sequence:
        if s[-1] == "-":
            label = s[:-1]
            box = hash(label)
            if label in boxes[box]:
                boxes[box].pop(label)

        if s[-2] == "=":
            label = s[:-2]
            box = hash(label)
            lens = int(s[-1])
            boxes[box][label] = lens

    # sum focusing power
    power = 0
    for b, box in boxes.items():
        for l, lens in enumerate(box.values()):
            power += (1 + b) * (1 + l) * lens

    print("Part 2:", power, f"(took {time() - t:.4f}s)")


if __name__ == "__main__":
    with open("input.txt") as f:
        sequence = parse_data(f)
    part_1(sequence)
    part_2(sequence)
