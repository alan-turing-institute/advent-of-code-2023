# AOC - day 2
#       _\/_
#        /\
#        /\
#       / \
#      /~~\o
#     /o  \
#    /~~*~~\
#  o/    o \
#  /~~~~~~~~\~`
# /__*_______\
#      ||
#    \====/
#     \__/

from typing import Dict, List

with open("input.txt") as f:
    lines: List[str] = f.readlines()

maxes: Dict[str, int] = {
    "red": 12,
    "green": 13,
    "blue": 14
}

total: int = 0
powersum: int = 0

for i, game in enumerate(lines):
    mins = {
        "red": 0,
        "green": 0,
        "blue": 0
    }
    sets: List[str] = game.split(": ")[1]
    valid: bool = True
    end: bool = False
    while valid and not end:
        for s in sets.split("; "):
            cubes: List[str] = s.strip("\n").split(", ")
            for cube in cubes:
                number: int = int(cube.split(" ")[0])
                colour: str = cube.split(" ")[1]
                if number > mins[colour]:
                    mins[colour] = number
                if number > maxes[colour]:
                    valid = False
        end = True

    if valid:
        total += i+1
    powersum += mins["red"] * mins["green"] * mins["blue"]

print("Part 1: " + str(total))
print("Part 2: " + str(powersum))