def parse_data(file, card_rankings, wildcard=None):
    hands = []
    for line in file.read().splitlines():
        cards, bid = line.split(" ")
        hands.append(Hand(cards, int(bid), card_rankings, wildcard))
    return hands


HAND_RANKINGS = [
    "5Kind",
    "4Kind",
    "FullHouse",
    "3Kind",
    "2Pair",
    "Pair",
    "HighCard",
]


class Hand:
    def __init__(
        self,
        cards: str,
        bid: int,
        card_rankings: list[str],
        wildcard: str | None = None,
    ):
        self.cards = cards
        self.bid = bid
        self.card_rankings = card_rankings
        self.wildcard = wildcard

    @property
    def kind(self):
        card_types = set(self.cards)
        card_counts = sorted(
            [self.cards.count(t) for t in card_types if t != self.wildcard],
            reverse=True,
        )
        wildcard_count = self.cards.count(self.wildcard) if self.wildcard else 0
        if len(card_counts) > 0:
            max_count = card_counts[0] + wildcard_count
        else:  # all cards are wildcards
            max_count = wildcard_count

        if max_count == 5:
            return "5Kind"
        if max_count == 4:
            return "4Kind"
        if max_count == 1:
            return "HighCard"

        # use any wildcards to try top of the most common card type up to 3
        need = 3 - card_counts[0]
        add_wilds = min([need, wildcard_count])
        card_counts[0] += add_wilds
        wildcard_count -= add_wilds
        # use any remaining wildcards to try to top up the 2nd most common type up to 2
        need = 2 - card_counts[1]
        add_wilds = min([need, wildcard_count])
        card_counts[1] += add_wilds
        wildcard_count -= add_wilds

        if 3 in card_counts and 2 in card_counts:
            return "FullHouse"
        if max_count == 3:
            return "3Kind"
        if card_counts.count(2) == 2:
            return "2Pair"
        if max_count == 2:
            return "Pair"
        raise ValueError("unexpected hand type")

    def __gt__(self, other):
        if HAND_RANKINGS.index(self.kind) < HAND_RANKINGS.index(other.kind):
            return True
        if HAND_RANKINGS.index(self.kind) > HAND_RANKINGS.index(other.kind):
            return False
        for me, you in zip(self.cards, other.cards):
            if self.card_rankings.index(me) < self.card_rankings.index(you):
                return True
            if self.card_rankings.index(me) > self.card_rankings.index(you):
                return False
        return False  # equal

    def __eq__(self, other):
        return self.cards == other.cards

    def __repr__(self):
        return f"Hand(cards={''.join(self.cards)}, bid={self.bid=}, kind={self.kind})"


def part_1(path):
    card_rankings = ["A", "K", "Q", "J", "T", "9", "8", "7", "6", "5", "4", "3", "2"]
    with open(path) as f:
        hands = parse_data(f, card_rankings)
    result = sum((rank + 1) * hand.bid for rank, hand in enumerate(sorted(hands)))
    print("Part 1:", result)


def part_2(path):
    card_rankings = ["A", "K", "Q", "T", "9", "8", "7", "6", "5", "4", "3", "2", "J"]
    with open(path) as f:
        hands = parse_data(f, card_rankings, wildcard="J")
    result = sum((rank + 1) * hand.bid for rank, hand in enumerate(sorted(hands)))
    print("Part 2:", result)


if __name__ == "__main__":
    part_1("input.txt")
    part_2("input.txt")
