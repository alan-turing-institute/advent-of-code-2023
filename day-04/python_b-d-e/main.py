


def part_one(cards):
    total = 0
    for card in cards:
        score = 0
        for candidate in card[1]:
            if candidate != "":
                if candidate in card[0]:
                    if score == 0:
                        score = 1
                    else:
                        score *= 2

        total += score

    print(total)


def part_two(cards):
    card_multiplier = [1] * len(cards)
    for i in range(len(cards)):
        num_matches = 0
        card = cards[i]
        for candidate in card[1]:
            if candidate != "":
                if candidate in card[0]:
                    num_matches += 1
        for j in range(i+1, i+num_matches+1):
            card_multiplier[j] += card_multiplier[i]

    print(sum(card_multiplier))



# get input
with open("input.txt") as f:
    data = f.read().splitlines()
cards = []
for line in data:
    line = line.split(": ")[1].split(" | ")
    winning = line[0].split(" ")
    candidates = line[1].split(" ")
    cards.append([winning, candidates])

part_one(cards)
part_two(cards)