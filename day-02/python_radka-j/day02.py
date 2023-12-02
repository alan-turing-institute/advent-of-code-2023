import math

# read the input file
with open('input02.txt') as f:
    lines = f.read().splitlines()

def get_game_sets(line_str):
    """
    Return list of dictionaries for each game subset in the format:
    [{colour:count, ...}, ...]
    """
    games = line_str.split(":")[-1]
    results = []
    
    for game_subsets in games.split(";"):
        game_subsets_clean = [res.strip().split() for res in game_subsets.split(",")]
        results.append({colour:int(count) for count,colour in game_subsets_clean})
    
    return results

# part 1
set_up = {"red": 12, "green": 13, "blue": 14}
possible_games = []
for i, line in enumerate(lines):

    game_id = i + 1
    game_subsets = get_game_sets(line)
    
    valid = True
    for subset in game_subsets:
        for colour,count in subset.items():
            if count > set_up[colour]:
                valid = False
    if valid:
        possible_games.append(game_id)

print(sum(possible_games))

# part 2
powers = []
for line in lines:
    min_cubes = {"red": 0, "green": 0, "blue": 0}

    game_subsets = get_game_sets(line)
    for subset in game_subsets:
        for colour,count in subset.items():
            min_cubes[colour] = max(count, min_cubes[colour])

    powers.append(math.prod(min_cubes.values()))

print(sum(powers))