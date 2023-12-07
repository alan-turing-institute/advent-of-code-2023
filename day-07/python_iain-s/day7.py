"""Solve the day's problem."""
import functools
from collections import Counter
from typing import Callable

the_map = {
    "A": 14,
    "K": 13,
    "Q": 12,
    "J": 11,
    "T": 10,
}

the_map_two = {
    "A": 14,
    "K": 13,
    "Q": 12,
    "J": 1,
    "T": 10,
}


def parse(lines: list[str]) -> tuple[list[str], list[int]]:
    temp = [line.split(" ") for line in lines]
    return [x[0] for x in temp], [int(x[1]) for x in temp]


def comparator_two(left: str, right: str) -> int:
    l_counter = Counter(left)
    r_counter = Counter(right)

    # always best to add joker to max value
    l_jokers = l_counter.pop("J", 0)
    l_max = max(l_counter.values()) if l_counter else 0
    for k, v in l_counter.items():
        if v == l_max:
            l_counter[k] = l_max + l_jokers
            break
    l_max = l_max + l_jokers

    r_jokers = r_counter.pop("J", 0)
    r_max = max(r_counter.values()) if r_counter else 0
    for k, v in r_counter.items():
        if v == r_max:
            r_counter[k] = r_max + r_jokers
            break
    r_max = r_max + r_jokers

    if l_max < r_max:
        return -1
    elif l_max > r_max:
        return 1
    else:
        if l_max < r_max:
            return -1
        if l_max > r_max:
            return 1

        # full house?
        if l_max == 3:
            l_min = min(l_counter.values())
            r_min = min(r_counter.values())

            assert l_min in [1, 2]
            assert r_min in [1, 2]

            if l_min < r_min:
                return -1
            elif l_min > r_min:
                return 1

        # two pair?
        if l_max == 2:
            l_pairs = len([x for x in l_counter if l_counter[x] == 2])
            r_pairs = len([x for x in r_counter if r_counter[x] == 2])

            if l_pairs < r_pairs:
                return -1
            elif l_pairs > r_pairs:
                return 1

        # tie-break
        for l, r in zip(left, right):
            if l == r:
                continue

            if l in the_map_two:
                l = the_map_two[l]
            else:
                l = int(l)

            if r in the_map_two:
                r = the_map_two[r]
            else:
                r = int(r)

            if l < r:
                return -1
            elif l > r:
                return 1

        raise Exception("tie")


def comparator(left: str, right: str) -> int:
    l_counter = Counter(left)
    r_counter = Counter(right)
    l_max = max(l_counter.values())
    r_max = max(r_counter.values())

    if l_max < r_max:
        return -1
    elif l_max > r_max:
        return 1
    else:
        # full house?
        if l_max == 3:
            l_min = min(l_counter.values())
            r_min = min(r_counter.values())

            assert l_min in [1, 2]
            assert r_min in [1, 2]

            if l_min < r_min:
                return -1
            elif l_min > r_min:
                return 1

        # two pair?
        if l_max == 2:
            l_pairs = len([x for x in l_counter if l_counter[x] == 2])
            r_pairs = len([x for x in r_counter if r_counter[x] == 2])

            if l_pairs < r_pairs:
                return -1
            elif l_pairs > r_pairs:
                return 1

        # tie-break
        for l, r in zip(left, right):
            if l == r:
                continue

            if l in the_map:
                l = the_map[l]
            else:
                l = int(l)

            if r in the_map:
                r = the_map[r]
            else:
                r = int(r)

            if l < r:
                return -1
            elif l > r:
                return 1

        raise Exception("tie")


def rank(hands: list[str]) -> list[str]:
    return sorted(hands, key=functools.cmp_to_key(comparator))


def rank_two(hands: list[str]) -> list[str]:
    return sorted(hands, key=functools.cmp_to_key(comparator_two))


def solve(lines: list[str], ranker: Callable[[list[str]], list[str]]) -> int:
    hands, bids = parse(lines)
    ranked = ranker(hands)
    result = []
    for i, x in enumerate(ranked, start=1):
        index = hands.index(x)
        bid = bids[index]
        result.append(i * bid)

    return sum(result)


def one(lines: list[str]) -> int:
    return solve(lines, rank)


def two(lines: list[str]) -> int:
    return solve(lines, rank_two)


def main() -> None:
    with open("./inputs/b/day7.txt", encoding="utf-8") as f:
        lines = [line.rstrip() for line in f]
    print("one:", one(lines))

    with open("./inputs/b/day7.txt", encoding="utf-8") as f:
        lines = [line.rstrip() for line in f]
    print("two:", two(lines))


if __name__ == "__main__":
    main()
