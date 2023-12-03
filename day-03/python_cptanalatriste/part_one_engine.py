from typing import Optional


def is_symbol(character: str) -> bool:
    return not character.isdigit() and character != "."


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
    # Check top-line
    for target_line_index in range(line_index - 1, line_index + 2):
        if 0 <= target_line_index < len(lines):
            target_line: str = lines[target_line_index]

            index_start: int = max(0, number_start_index - 1)
            index_end: int = min(len(target_line) - 1, number_end_index + 1)

            if look_for_symbol(target_line, index_start, index_end):
                line_with_number: str = lines[line_index]
                part_numbers.append(
                    int(
                        line_with_number[
                            number_start_index : number_end_index + 1
                        ]
                    )
                )
                return


def get_part_numbers(lines: list[str]) -> list[int]:
    part_numbers: list[int] = []
    for line_index, line in enumerate(lines):
        number_start_index: Optional[int] = None

        for character_index, character in enumerate(line):
            if character.isdigit() and number_start_index is None:
                number_start_index = character_index

            if not character.isdigit() and number_start_index is not None:
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
                character.isdigit()
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


def main():
    lines: str = []
    with open("input.txt") as input_file:
        while line := input_file.readline():
            lines.append(line)

    part_numbers: list[int] = get_part_numbers(lines)

    print(f"answer={sum(part_numbers)}")


if __name__ == "__main__":
    main()
