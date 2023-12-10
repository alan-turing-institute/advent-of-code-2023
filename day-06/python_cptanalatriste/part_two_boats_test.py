from python_cptanalatriste import part_two_boats


def test_parse_file() -> None:
    assert (71530, 940200) == part_two_boats.parse_file(
        "day-06/python_cptanalatriste/sample_input.txt"
    )


def test_process_input() -> None:
    assert 71503 == part_two_boats.process_input(
        "day-06/python_cptanalatriste/sample_input.txt"
    )
