from python_cptanalatriste import part_one_boats


def parse_file(file_name: str) -> tuple[int, int]:
    times_values, distances_values = part_one_boats.get_time_distance_lists(
        file_name
    )
    times = [str(time_value) for time_value in times_values]
    distances = [str(distance_value) for distance_value in distances_values]
    return int("".join(times)), int("".join(distances))


def process_input(file_name: str) -> int:
    total_time, total_distance = parse_file(file_name)
    conditions: list[int] = part_one_boats.get_winning_conditions(
        total_time, total_distance
    )
    return len(conditions)


if __name__ == "__main__":
    print(process_input("python_cptanalatriste/input.txt"))
