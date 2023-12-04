from python_cptanalatriste import part_two_engine
from python_cptanalatriste import part_one_engine_test


def test_get_gears() -> None:
    gears: list[tuple] = part_two_engine.get_gears(
        part_one_engine_test.INPUT_AS_LINES
    )

    assert len(gears) == 2
    assert (467, 35) in gears
    assert (755, 598) in gears


def test_reading_input() -> None:
    assert 467835 == part_two_engine.process_file(
        "day-03/python_cptanalatriste/sample_input.txt"
    )

def test_sample_grid() -> None:
    assert 6756 == part_two_engine.process_file(
        "day-03/python_cptanalatriste/sample_grid.txt"
    )
