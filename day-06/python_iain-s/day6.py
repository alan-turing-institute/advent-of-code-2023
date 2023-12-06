"""Solve the day's problem."""
import operator
from functools import reduce


def winning_strategies(race: tuple[int, int]) -> list[int]:
    duration, record_distance = race

    # brute force
    result = []
    for hold_seconds in range(duration):
        distance = hold_seconds * (duration - hold_seconds)
        if distance > record_distance:
            result.append(hold_seconds)

    return result


def get_answer(durations: list[int], record_distances: list[int]) -> int:
    races = zip(durations, record_distances)
    strategies = map(winning_strategies, races)
    count_strategies = map(len, strategies)
    product_count_strategies = reduce(operator.mul, count_strategies)
    return product_count_strategies


def one(lines: list[str]) -> int:
    durations = [int(x) for x in lines[0].split(" ") if x.isdigit()]
    record_distances = [int(x) for x in lines[1].split(" ") if x.isdigit()]
    return get_answer(durations, record_distances)


def two(lines: list[str]) -> int:
    durations = [
        int(x.replace(" ", ""))
        for x in lines[0].split(":")
        if x.replace(" ", "").isdigit()
    ]
    record_distances = [
        int(x.replace(" ", ""))
        for x in lines[1].split(":")
        if x.replace(" ", "").isdigit()
    ]
    return get_answer(durations, record_distances)


def main() -> None:
    with open("./inputs/b/day6.txt", encoding="utf-8") as f:
        lines = [line.rstrip() for line in f]
    print("one:", one(lines))

    with open("./inputs/b/day6.txt", encoding="utf-8") as f:
        lines = [line.rstrip() for line in f]
    print("two:", two(lines))


if __name__ == "__main__":
    main()
