from python_cptanalatriste import part_one_boats


def test_parse_input() -> None:
    expected_output: list[tuple[int, int]] = [(7, 9), (15, 40), (30, 200)]

    assert expected_output == part_one_boats.parse_file(
        "day-06/python_cptanalatriste/sample_input.txt"
    )


def test_distance_by_button_hold() -> None:
    total_time: int = 7
    assert 0 == part_one_boats.get_distance_by_button_hold(
        button_hold=0, total_time=total_time
    )

    assert 6 == part_one_boats.get_distance_by_button_hold(
        button_hold=1, total_time=total_time
    )

    assert 10 == part_one_boats.get_distance_by_button_hold(
        button_hold=2, total_time=total_time
    )

    assert 0 == part_one_boats.get_distance_by_button_hold(
        button_hold=7, total_time=total_time
    )


def test_winning_conditions() -> None:
    assert [2, 3, 4, 5] == part_one_boats.get_winning_conditions(
        total_time=7, current_distance=9
    )

    winning_conditions: list[int] = part_one_boats.get_winning_conditions(
        total_time=15, current_distance=40
    )
    assert 4 in winning_conditions
    assert 11 in winning_conditions
    assert 8 == len(winning_conditions)

    winning_conditions = part_one_boats.get_winning_conditions(
        total_time=30, current_distance=200
    )
    assert 11 in winning_conditions
    assert 19 in winning_conditions
    assert 9 == len(winning_conditions)


def test_process_input() -> None:
    assert 288 == part_one_boats.process_input(
        "day-06/python_cptanalatriste/sample_input.txt"
    )
