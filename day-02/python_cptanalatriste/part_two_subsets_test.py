from python_cptanalatriste.part_one_subsets import parse_line, Subset
from python_cptanalatriste.part_two_subsets import calculate_minimum_bag


def test_minimum_bag() -> None:
    _, subsets = parse_line("Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green")
    result: Subset = calculate_minimum_bag(subsets)
    assert Subset(red=4, green=2, blue=6) == result

    _, subsets = parse_line(
        "Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue"
    )
    result: Subset = calculate_minimum_bag(subsets)
    assert Subset(red=1, green=3, blue=4) == result

    _, subsets = parse_line(
        "Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red"
    )
    result: Subset = calculate_minimum_bag(subsets)
    assert Subset(red=20, green=13, blue=6) == result

    _, subsets = parse_line(
        "Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red"
    )
    result: Subset = calculate_minimum_bag(subsets)
    assert Subset(red=14, green=3, blue=15) == result

    _, subsets = parse_line(
        "Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green"
    )
    result: Subset = calculate_minimum_bag(subsets)
    assert Subset(red=6, green=3, blue=2) == result
