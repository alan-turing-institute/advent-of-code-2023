color_to_idx = {
    "red": 0,
    "green": 1,
    "blue": 2,
}


def parse_game(game):
    draws = game.split(": ")[1].split("; ")
    return [parse_draw(draw) for draw in draws]


def parse_draw(draw):
    cubes = draw.split(", ")
    counts = [0, 0, 0]
    for cube in cubes:
        number, color = cube.split(" ")
        counts[color_to_idx[color.strip()]] = int(number)
    return counts


def part_1(games):
    bag = [12, 13, 14]
    total = 0
    for game_id, game in enumerate(games):
        game_ok = True
        for round in game:
            if any(found > available for found, available in zip(round, bag)):
                game_ok = False
                break
        if game_ok:
            total += game_id + 1

    print("Part 1:", total)


def part_2(games):
    total = 0
    for game in games:
        min_cubes = [0, 0, 0]
        for round in game:
            for idx, count in enumerate(round):
                if count > min_cubes[idx]:
                    min_cubes[idx] = count
        power = 1
        for c in min_cubes:
            if c > 0:
                power *= c
        total += power

    print("Part 2:", total)


if __name__ == "__main__":
    with open("input.txt") as f:
        games = [parse_game(game) for game in f.readlines()]

    part_1(games)
    part_2(games)
