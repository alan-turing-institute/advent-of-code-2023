import re


def parse_data(file):
    cards = []
    for line in file.read().splitlines():
        winning, picks = line.split(": ")[1].split(" | ")
        winning = re.findall(r"\d+", winning)
        picks = re.findall(r"\d+", picks)
        cards.append({"winning": winning, "picks": picks, "count": 1})
    return cards


def part_1(cards):
    pts = 0
    for card in cards:
        correct = set(card["winning"]) & set(card["picks"])
        if len(correct) > 0:
            pts += 2 ** (len(correct) - 1)

    print("Part 1:", pts)


def part_2(cards):
    for game_id, card in enumerate(cards):
        correct = set(card["winning"]) & set(card["picks"])
        for delta in range(1, len(correct) + 1):
            if game_id + delta >= len(cards):
                break
            cards[game_id + delta]["count"] += cards[game_id]["count"]

    print("Part 2:", sum(c["count"] for c in cards))


if __name__ == "__main__":
    with open("input.txt") as f:
        cards = parse_data(f)

    part_1(cards)
    part_2(cards)
