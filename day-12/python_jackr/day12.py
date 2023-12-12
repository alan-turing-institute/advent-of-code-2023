from copy import deepcopy
from time import time


def parse_data(file) -> (list[list[str]], list[int]):
    lines = file.read().splitlines()
    springs = []
    group_counts = []
    for line in lines:
        spr, grp = line.split()
        springs.append(list(spr))
        group_counts.append([int(g) for g in grp.split(",")])
    return springs, group_counts


def get_groups(springs: list) -> list:
    # "".join(springs) converts springs to a str
    # list(filter(None, x.split(".")) drops empty strings after the split (due to
    # multiple '.' in a row)
    return list(filter(None, "".join(springs).split(".")))


def count_groups(groups):
    return [len(g) for g in groups]


def permutations(spring, n_damaged):
    candidates = [spring]
    for idx in range(len(spring)):
        if spring[idx] == "?":
            new_candidates = []
            for c in candidates:
                if c.count("#") < n_damaged:
                    states = ["#", "."]
                else:
                    states = ["."]
                for state in states:
                    new_c = deepcopy(c)
                    new_c[idx] = state
                    new_candidates.append(new_c)
            candidates = new_candidates

    return candidates


def part_1(springs, group_counts):
    t = time()
    total = 0
    for idx, vals in enumerate(zip(springs, group_counts)):
        spring, exp_groups = vals
        n_damaged = sum(exp_groups)

        for p in permutations(spring, n_damaged):
            act_groups = count_groups(get_groups(p))
            if act_groups == exp_groups:
                total += 1

    print("Part 1:", total, time() - t)


def part_1_2(springs, group_counts):
    t = time()
    total = 0
    for idx, vals in enumerate(zip(springs, group_counts)):
        spring, exp_counts = vals
        groups = get_groups(spring)
        exp_n_damaged = sum(exp_counts)
        exp_n_groups = len(exp_counts)
        print(spring)
        print(groups)
        print(exp_n_damaged)
        print(exp_n_groups)

    print("Part 1:", total, time() - t)


def part_2(springs, group_counts):
    t = time()
    total = 0
    for idx, vals in enumerate(zip(springs, group_counts)):
        spring, exp_groups = vals
        spring *= 5
        exp_groups *= 5
        print(idx)
        n_damaged = sum(exp_groups)
        for p in permutations(spring, n_damaged):
            act_groups = count_groups(get_groups(p))
            if act_groups == exp_groups:
                total += 1
    print("Part 2:", time() - t)


if __name__ == "__main__":
    with open("input.txt") as f:
        springs, group_counts = parse_data(f)
    part_1(springs, group_counts)
    #  part_2(springs, group_counts)
