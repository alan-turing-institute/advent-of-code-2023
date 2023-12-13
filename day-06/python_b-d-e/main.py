def parse_input(fname):
    lines = [list(filter(None, line.split(" "))) for line in open(fname).read().splitlines()]
    return  [{"time": int(lines[0][i]), "distance": int(lines[1][i])} for i in range(1, len(lines[0]))]

def part_one(data):
    error = 1
    for race in data:
        lowest, highest = 0, race['time']
        while lowest * (race['time'] - lowest) <= race['distance']:
            lowest += 1
        while highest * (race['time'] - highest) <= race['distance']:
            highest -= 1
        error *= (highest - lowest + 1)
    print(error)


# data = parse_input("example.txt")
data = parse_input("input.txt")
part_one(data)