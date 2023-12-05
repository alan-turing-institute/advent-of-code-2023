def calculate_points(winning_numbers: set[int], your_numbers: set[int]) -> int:
    matching_numbers: set[int] = winning_numbers.intersection(your_numbers)
    if len(matching_numbers) == 0:
        return 0
    return 2 ** (len(matching_numbers) - 1)


def string_to_set(line: str) -> set[int]:
    return {
        int(number_string)
        for number_string in line.split(" ")
        if number_string.isdigit()
    }


def parse_line(line: str) -> tuple[int, set[int], set[int]]:
    card_and_number, all_numbers = line.split(":")
    card_number: int = int(card_and_number[5:])

    winning_numbers_string, your_numbers_string = all_numbers.split("|")
    winning_numbers: set[int] = string_to_set(winning_numbers_string)
    your_numbers: set[int] = string_to_set(your_numbers_string)

    return card_number, winning_numbers, your_numbers


def process_input_file(file_name: str) -> int:
    answer: int = 0
    with open(file_name) as input_file:
        while line := input_file.readline():
            _, winning_numbers, your_numbers = parse_line(line.rstrip("\n"))
            card_points: int = calculate_points(winning_numbers, your_numbers)
            answer += card_points
            print(f"{card_points=} {line}")

    return answer


if __name__ == "__main__":
    print(process_input_file("day-04/python_cptanalatriste/input.txt"))
