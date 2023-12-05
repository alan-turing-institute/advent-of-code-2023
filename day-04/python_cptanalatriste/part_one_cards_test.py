from python_cptanalatriste import part_one_cards

LINES: list[str] = [
    "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53",
    "Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19",
    "Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1",
    "Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83",
    "Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36",
    "Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11",
]


def test_parse_line() -> None:
    winning_numbers, your_numbers = part_one_cards.parse_line(LINES[0])
    assert {41, 48, 83, 86, 17} == winning_numbers
    assert {83, 86, 6, 31, 17, 9, 48, 53} == your_numbers


def test_calculate_points() -> None:
    winning_numbers, your_numbers = part_one_cards.parse_line(LINES[0])
    assert 8 == part_one_cards.calculate_points(winning_numbers, your_numbers)

    winning_numbers, your_numbers = part_one_cards.parse_line(LINES[1])
    assert 2 == part_one_cards.calculate_points(winning_numbers, your_numbers)

    winning_numbers, your_numbers = part_one_cards.parse_line(LINES[2])
    assert 2 == part_one_cards.calculate_points(winning_numbers, your_numbers)

    winning_numbers, your_numbers = part_one_cards.parse_line(LINES[3])
    assert 1 == part_one_cards.calculate_points(winning_numbers, your_numbers)

    winning_numbers, your_numbers = part_one_cards.parse_line(LINES[4])
    assert 0 == part_one_cards.calculate_points(winning_numbers, your_numbers)

    winning_numbers, your_numbers = part_one_cards.parse_line(LINES[5])
    assert 0 == part_one_cards.calculate_points(winning_numbers, your_numbers)


def test_process_input_file() -> None:
    assert 13 == part_one_cards.process_input_file(
        "day-04/python_cptanalatriste/sample_input.txt"
    )


def test_float_points() -> None:
    winning_numbers, your_numbers = part_one_cards.parse_line(
        "Card   5: 76 99 87 97 16 61 73 20 22 19 | 83 27 89 80  1 84 23 21 35 12 60 17 63 26 88  3 70 49  6 34 30 77 59 82 66"
    )
    assert 0 == part_one_cards.calculate_points(winning_numbers, your_numbers)
