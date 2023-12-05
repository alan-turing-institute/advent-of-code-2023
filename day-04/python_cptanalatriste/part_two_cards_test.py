from python_cptanalatriste import part_one_cards_test
from python_cptanalatriste import part_two_cards


def test_process_card() -> None:
    all_cards: dict[int, part_two_cards.Card] = {
        part_two_cards.card_from_line(line)[
            "number"
        ]: part_two_cards.card_from_line(line)
        for line in part_one_cards_test.LINES
    }

    copied_cards: list[int] = part_two_cards.process_card(1, all_cards)
    expected_copies: list[int] = [2, 3, 4, 5]
    assert expected_copies == copied_cards

    copied_cards = part_two_cards.process_card(2, all_cards)
    expected_copies = [3, 4]
    assert expected_copies == copied_cards

    copied_cards = part_two_cards.process_card(6, all_cards)
    expected_copies = []
    assert expected_copies == copied_cards


def test_process_deck() -> None:
    all_cards: dict[int, part_two_cards.Card] = {
        part_two_cards.card_from_line(line)[
            "number"
        ]: part_two_cards.card_from_line(line)
        for line in part_one_cards_test.LINES
    }

    card_counters: dict[int, int] = part_two_cards.process_deck(all_cards)
    expected_counters: dict[int, int] = {1: 1, 2: 2, 3: 4, 4: 8, 5: 14, 6: 1}

    assert expected_counters == card_counters
    assert 30 == sum(card_counters.values())
    assert 30 == sum(expected_counters.values())


def test_process_file() -> None:
    assert 30 == part_two_cards.process_file(
        "day-04/python_cptanalatriste/sample_input.txt"
    )
