from typing import Optional, Callable


def get_calibration_value(line: str) -> int:
    first_digit: Optional[str] = None
    second_digit: Optional[str] = None

    left_index: int = 0
    right_index: int = len(line) - 1

    while left_index <= right_index:
        left_character: str = line[left_index]
        right_character: str = line[right_index]

        if left_character.isdigit() and first_digit is None:
            first_digit = left_character
        if right_character.isdigit() and second_digit is None:
            second_digit = right_character

        if first_digit is None:
            left_index += 1
        if second_digit is None:
            right_index -= 1

        if first_digit is not None and second_digit is not None:
            break

    if first_digit is None or second_digit is None:
        raise Exception(f"Cannot process line: {line}. {first_digit=} {second_digit=}")

    return int(first_digit + second_digit)


def main(parsing_function: Callable[[str], int]):
    answer: int = 0
    with open("input.txt") as input_file:
        while line := input_file.readline():
            answer += parsing_function(line)

    print(f"{answer=}")


if __name__ == "__main__":
    main(parsing_function=get_calibration_value)
