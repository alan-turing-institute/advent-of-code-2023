from python_cptanalatriste import part_one_engine


def check_number_and_gear(
    gear_index: int,
    line_index: int,
    number_start_index: int,
    number_end_index: int,
    lines: list[str],
    numbers: list[int],
) -> None:
    line_with_number: str = lines[line_index]
    number: int = int(
        line_with_number[number_start_index : number_end_index + 1]
    )

    if (number_start_index - 1) <= gear_index <= number_end_index + 1:
        numbers.append(number)


def check_gear(
    character_line: int,
    character_index: int,
    lines: list[str],
    gears: list[tuple],
) -> None:
    print(f"Checking gear in {character_line=} and {character_index=}")
    parts_around_gear: list[int] = []

    for target_line_index in range(character_line - 1, character_line + 2):

        def processing_function(
            line_index: int,
            number_start_index: int,
            number_end_index: int,
            lines: list[str],
            parts_around_gear: list[int],
        ) -> None:
            print(
                f"Number {lines[line_index][number_start_index: number_end_index + 1]}"
                f" detected at line {line_index=}"
            )
            check_number_and_gear(
                character_index,
                line_index,
                number_start_index,
                number_end_index,
                lines,
                parts_around_gear,
            )

        if 0 <= target_line_index < len(lines):
            part_one_engine.process_line(
                target_line_index,
                lines,
                parts_around_gear,
                processing_function,
            )

    if len(parts_around_gear) == 2:
        gears.append(tuple(parts_around_gear))

    print(f"Processed {target_line_index=}. Parts : {parts_around_gear}")


def get_gears(lines: list[str]) -> list[tuple]:
    gears: list[tuple] = []
    for line_index, line in enumerate(lines):
        for character_index, character in enumerate(line):
            if character == "*":
                check_gear(line_index, character_index, lines, gears)

    return gears


def process_file(file_name: str) -> int:
    lines: list[str] = part_one_engine.get_lines(file_name)
    gears: list[tuple] = get_gears(lines)

    print(f"{gears=}")

    return sum(
        [
            first_part_number * second_part_number
            for first_part_number, second_part_number in gears
        ]
    )


def main():
    print(f"answer={process_file('python_cptanalatriste/input.txt')}")


if __name__ == "__main__":
    main()
