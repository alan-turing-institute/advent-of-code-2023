def parse_data(file):
    data = []
    for line in file.read().splitlines():
        numbers = [int(n) for n in line.split()]
        data.append([int(n) for n in numbers])
    return data


def differences(series):
    return [series[idx] - series[idx - 1] for idx in range(1, len(series))]


def get_next(series):
    last_value = series[-1]
    while not all(n == 0 for n in series):
        series = differences(series)
        last_value += series[-1]
    return last_value


def get_previous(series):
    diffs = [series[0]]
    while not all(n == 0 for n in series):
        series = differences(series)
        diffs.append(series[0])
    prevs = [None] * len(diffs)
    prevs[-1] = 0
    for idx in reversed(range(len(diffs) - 1)):
        prevs[idx] = diffs[idx] - prevs[idx + 1]
    return prevs[0]


def part_1(data):
    print("Part 1:", sum(get_next(s) for s in data))


def part_2(data):
    print("Part 2:", sum(get_previous(s) for s in data))


if __name__ == "__main__":
    with open("input.txt") as f:
        data = parse_data(f)
    part_1(data)
    part_2(data)
