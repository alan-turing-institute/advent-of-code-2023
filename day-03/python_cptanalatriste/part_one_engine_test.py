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
