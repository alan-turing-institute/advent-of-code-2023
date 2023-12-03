from python_cptanalatriste import part_one_engine


def test_get_part_numbers() -> None:
    lines: list[str] = [
        "467..114..",
        "...*......",
        "..35..633.",
        "......#...",
        "617*......",
        ".....+.58.",
        "..592.....",
        "......755.",
        "...$.*....",
        ".664.598..",
    ]

    part_numbers: list[int] = part_one_engine.get_part_numbers(lines)

    assert 467 in part_numbers
    assert 114 not in part_numbers
    assert 35 in part_numbers
    assert 633 in part_numbers
    assert 617 in part_numbers
    assert 58 not in part_numbers
    assert 592 in part_numbers
    assert 755 in part_numbers
    assert 664 in part_numbers
    assert 598 in part_numbers

    assert 4361 == sum(part_numbers)


def test_grid_from_reddit() -> None:
    """
    From: https://www.reddit.com/r/adventofcode/comments/189q9wv/2023_day_3_another_sample_grid_to_use/
    """
    lines: list[str] = [
        "12.......*..",
        "+.........34",
        ".......-12..",
        "..78........",
        "..*....60...",
        "78..........",
        ".......23...",
        "....90*12...",
        "............",
        "2.2......12.",
        ".*.........*",
        "1.1.......56",
    ]

    part_numbers: list[int] = part_one_engine.get_part_numbers(lines)
    assert 413 == sum(part_numbers)
    assert 90 in part_numbers
    assert 12 in part_numbers
    assert 60 not in part_numbers


def test_obvious_one() -> None:
    lines: list[str] = [
        "........",
        ".24..4..",
        "......*.",
    ]

    part_numbers: list[int] = part_one_engine.get_part_numbers(lines)
    assert 24 not in part_numbers
    assert 4 in part_numbers


def test_reading_input() -> None:
    assert 4361 == part_one_engine.process_file(
        "day-03/python_cptanalatriste/sample_input.txt"
    )


def test_getting_lines() -> None:
    lines: list[str] = part_one_engine.get_lines(
        "day-03/python_cptanalatriste/sample_input.txt"
    )

    assert "467..114.." == lines[0]
    assert "...*......" == lines[1]
    assert "..35..633." == lines[2]
    assert "......#..." == lines[3]


def test_dollars() -> None:
    lines: list[str] = [
        "$......$",
        ".11..11.",
        ".11.11..",
        "$......$",
    ]

    part_numbers: list[int] = part_one_engine.get_part_numbers(lines)
    assert 33 == sum(part_numbers)


def test_the_one_I_needed() -> None:
    lines: list[str] = [
        "97..",
        "...*",
        "100.",
    ]

    part_numbers: list[int] = part_one_engine.get_part_numbers(lines)
    assert 100 == sum(part_numbers)
