def get_time_distance_lists(file_name: str) -> tuple[list, list]:
    times: list[int] = []
    distances: list[int] = []
    with open(file_name) as input_file:
        for line in input_file:
            line = line.rstrip("\n")
            _, values_as_string = line.split(":")
            value_list: list[int] = [
                int(value) for value in values_as_string.split()
            ]

            if line.startswith("Time:"):
                times = value_list
            elif line.startswith("Distance:"):
                distances = value_list

    return times, distances


def parse_file(file_name: str) -> list[tuple[int, int]]:
    times, distances = get_time_distance_lists(file_name)
    return [(time, distance) for time, distance in zip(times, distances)]


def get_distance_by_button_hold(button_hold: int, total_time: int) -> int:
    travel_time: int = total_time - button_hold
    velocity: int = button_hold

    return velocity * travel_time


def get_winning_conditions(
    total_time: int, current_distance: int
) -> list[int]:
    return [
        button_hold
        for button_hold in range(0, total_time + 1)
        if get_distance_by_button_hold(button_hold, total_time)
        > current_distance
    ]


def process_input(file_name: str) -> int:
    answer: int = 1
    for total_time, total_distance in parse_file(file_name):
        answer *= len(get_winning_conditions(total_time, total_distance))

    return answer


if __name__ == "__main__":
    print(process_input("day-06/python_cptanalatriste/input.txt"))
