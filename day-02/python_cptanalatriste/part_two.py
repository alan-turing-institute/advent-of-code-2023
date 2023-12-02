from part_one import Subset, parse_line


def calculate_minimum_bag(subsets: list[Subset]) -> Subset:
    minimum_bag: Subset = Subset(red=0, blue=0, green=0)

    for subset in subsets:
        for key in ["blue", "red", "green"]:
            minimum_bag[key] = max(minimum_bag[key], subset[key])

    return minimum_bag


def main() -> None:
    answer: int = 0
    with open("input.txt") as input_file:
        while line := input_file.readline():
            game_id, subsets = parse_line(line)

            minimum_bag: Subset = calculate_minimum_bag(subsets)
            answer += minimum_bag["red"] * minimum_bag["blue"] * minimum_bag["green"]

    print(f"{answer=}")


if __name__ == "__main__":
    main()
