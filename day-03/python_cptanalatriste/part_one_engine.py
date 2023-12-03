from typing import Optional

DIGITS: list[str] = ["0", "1", "2", "3", "4", "5", "6", "7", "8", "9"]


def is_symbol(character: str) -> bool:
    return (character not in DIGITS) and character != "."


def look_for_symbol(line: str, index_start: int, index_end: int) -> bool:
    for index, character in enumerate(line):
        if index_start <= index <= index_end and is_symbol(character):
            return True

    return False


def check_number(
    line_index: int,
    number_start_index: int,
    number_end_index: int,
    lines: list[str],
    part_numbers: list[int],
) -> None:
    line_with_number: str = lines[line_index]
    number: int = int(
        line_with_number[number_start_index : number_end_index + 1]
    )
    for target_line_index in range(line_index - 1, line_index + 2):
        if 0 <= target_line_index < len(lines):
            target_line: str = lines[target_line_index]

            index_start: int = max(0, number_start_index - 1)
            index_end: int = min(len(target_line) - 1, number_end_index + 1)

            if look_for_symbol(target_line, index_start, index_end):
                part_numbers.append(number)
                print(f"Line {line_index + 1}: Adding {number=}")
                return

    print(f"Line {line_index + 1}: Ignoring {number=}")


def get_part_numbers(lines: list[str]) -> list[int]:
    part_numbers: list[int] = []
    for line_index, line in enumerate(lines):
        number_start_index: Optional[int] = None

        for character_index, character in enumerate(line):
            if character in DIGITS and number_start_index is None:
                number_start_index = character_index

            if (character not in DIGITS) and number_start_index is not None:
                number_end_index: int = character_index - 1
                check_number(
                    line_index,
                    number_start_index,
                    number_end_index,
                    lines,
                    part_numbers,
                )

                number_start_index = None

            # Line finished. Check if there's a character pending.
            if (
                character in DIGITS
                and number_start_index is not None
                and character_index == len(line) - 1
            ):
                number_end_index = character_index
                check_number(
                    line_index,
                    number_start_index,
                    number_end_index,
                    lines,
                    part_numbers,
                )
                number_start_index = None

    return part_numbers


def get_lines(file_name: str) -> list[str]:
    lines: list[str] = []
    with open(file_name) as input_file:
        while line := input_file.readline():
            lines.append(line.rstrip("\n"))

    return lines


def process_file(file_name: str) -> int:
    lines: list[str] = get_lines(file_name)
    part_numbers: list[int] = get_part_numbers(lines)
    return sum(part_numbers)


def main():
    print(f"answer={process_file('input.txt')}")


if __name__ == "__main__":
    main()
