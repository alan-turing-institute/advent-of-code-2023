from typing import TypedDict


class Subset(TypedDict):
    red: int
    blue: int
    green: int


def is_subset_possible(bag: Subset, subset: Subset) -> bool:
    for key in ["blue", "red", "green"]:
        if bag[key] < subset[key]:
            return False

    return True


def is_possible(bag: Subset, subsets: list[Subset]) -> bool:
    for subset in subsets:
        if not is_subset_possible(bag, subset):
            return False

    return True


def parse_line(line: str) -> tuple[int, list[Subset]]:
    parts: list[str] = line.split(":")
    game_id_string: str = parts[0]
    game_id_string = game_id_string[5:]

    subsets: list[Subset] = []
    subsets_string: list[str] = parts[1].split(";")
    for subset_string in subsets_string:
        elements_strings: list[str] = subset_string.split(",")

        subset: Subset = Subset(red=0, blue=0, green=0)
        for element in elements_strings:
            for key in ["blue", "red", "green"]:
                if key in element:
                    subset[key] = int(element[: -(len(key) + 1)])
        subsets.append(subset)

    return int(game_id_string), subsets


def main() -> None:
    answer: int = 0
    bag = Subset(red=12, green=13, blue=14)
    with open("input.txt") as input_file:
        while line := input_file.readline():
            game_id, subsets = parse_line(line)

            if is_possible(bag, subsets):
                answer += game_id

    print(f"{answer=}")


if __name__ == "__main__":
    main()
