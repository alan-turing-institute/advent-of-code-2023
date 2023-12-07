from python_cptanalatriste import part_one_seeds
from typing import Optional


def get_ranges(seed_ranges: list[int]) -> list[tuple[int, int]]:
    ranges: list[tuple[int, int]] = [
        (seed_ranges[index], seed_ranges[index + 1])
        for index in range(0, len(seed_ranges), 2)
    ]

    ranges = sorted(
        [
            (range_start, range_start + range_size)
            for range_start, range_size in ranges
        ],
        key=lambda start_and_end: start_and_end[0],
    )

    return ranges


def process_seed_ranges(
    seed_ranges: list[int], analyser: part_one_seeds.SeedAnaliser
) -> int:
    minimum_location: Optional[int] = None

    ranges: list[tuple[int, int]] = get_ranges(seed_ranges)

    for range_index, current_range in enumerate(ranges):
        range_start, range_end = current_range
        # print(f"{range_start=} {range_end=}")
        for seed in range(range_start, range_end):
            seed_location: int = analyser.analyse_seed(seed)["location"]
            print(f"{range_index=} {seed=} {seed_location=}")
            if minimum_location is None or seed_location < minimum_location:
                minimum_location = seed_location

    if minimum_location is not None:
        return minimum_location

    return -1


def process_in_reverse(
    seed_ranges: list[int], analyser: part_one_seeds.SeedAnaliser
) -> int:
    minimum_location: int = 0

    ranges: list[tuple[int, int]] = get_ranges(seed_ranges)

    while True:
        current_seed: int = analyser.analyse_location(minimum_location)["seed"]
        # print(f"{minimum_location=} {current_seed=}")

        for range_index, current_range in enumerate(ranges):
            range_start, range_end = current_range
            if range_start <= current_seed <= range_end:
                return minimum_location

        minimum_location += 1

    return -1


def main(file_name: str) -> int:
    print("Starting parsing...")
    analyser, seeds = part_one_seeds.process_input_file(file_name)
    print("Parsing ended...")

    # return process_seed_ranges(seeds, analyser)
    return process_in_reverse(seeds, analyser)


if __name__ == "__main__":
    print("Starting...")
    print(main("python_cptanalatriste/input.txt"))
    print("Finished!")
