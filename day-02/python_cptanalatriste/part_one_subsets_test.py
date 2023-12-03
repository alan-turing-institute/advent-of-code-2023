from python_cptanalatriste.part_one_subsets import Subset, parse_line, is_possible


def test_subsets() -> None:
    bag: Subset = Subset(red=12, green=13, blue=14)

    game_id, subsets = parse_line(
        "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green"
    )

    assert is_possible(bag, subsets)

    game_id, subsets = parse_line(
        "Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue"
    )

    assert is_possible(bag, subsets)

    game_id, subsets = parse_line(
        "Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green"
    )

    assert is_possible(bag, subsets)

    game_id, subsets = parse_line(
        "Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red"
    )

    assert not is_possible(bag, subsets)

    game_id, subsets = parse_line(
        "Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red"
    )

    assert not is_possible(bag, subsets)
