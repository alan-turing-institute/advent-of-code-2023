from typing import TypedDict
from python_cptanalatriste import part_one_cards


class Card(TypedDict):
    number: int
    winning_numbers: set[int]
    your_numbers: set[int]


def process_card(card_number: int, all_cards: dict[int, Card]) -> list[int]:
    card: Card = all_cards[card_number]

    matching_numbers: int = len(
        card["winning_numbers"].intersection(card["your_numbers"])
    )

    first_card_number: int = card["number"] + 1
    return [
        card_number
        for card_number in range(
            first_card_number, first_card_number + matching_numbers
        )
    ]


def process_deck(all_cards: dict[int, Card]) -> dict[int, int]:
    card_counters: dict[int, int] = {
        card["number"]: 1 for card in all_cards.values()
    }

    for card_number in sorted(all_cards.keys()):
        current_card_copies: int = card_counters[card_number]
        # print(f"{card_number=} {current_card_copies=}")
        for _ in range(current_card_copies):
            copies: list[int] = process_card(card_number, all_cards)
            for copy_number in copies:
                card_counters[copy_number] += 1

    # print(f"{card_counters=}")
    return card_counters


def card_from_line(line: str) -> Card:
    card_number, winning_numbers, your_numbers = part_one_cards.parse_line(
        line
    )

    return Card(
        number=card_number,
        winning_numbers=winning_numbers,
        your_numbers=your_numbers,
    )


def process_file(file_name: str) -> int:
    deck: dict[int, Card] = {}
    with open(file_name) as input_file:
        while line := input_file.readline():
            card: Card = card_from_line(line.rstrip("\n"))
            deck[card["number"]] = card

    counters: dict[int, int] = process_deck(deck)
    return sum(counters.values())


if __name__ == "__main__":
    print(f"answer={process_file('python_cptanalatriste/input.txt')}")
