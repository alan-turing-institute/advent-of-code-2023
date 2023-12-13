from time import time


def parse_data(file) -> (list[list[str]], list[int]):
    patterns = file.read().split("\n\n")
    patterns = [p.splitlines() for p in patterns]
    return patterns


def is_reflected(pattern, row_idx):
    if row_idx == 0:
        return False

    top = list(reversed(pattern[:row_idx]))
    bottom = pattern[row_idx:]
    n = min((len(top), len(bottom)))
    top = top[:n]
    bottom = bottom[:n]

    return top == bottom


def reflection_if_smudge(pattern, row_idx):
    if row_idx == 0:
        return False

    top = list(reversed(pattern[:row_idx]))
    bottom = pattern[row_idx:]
    n = min((len(top), len(bottom)))
    top = top[:n]
    bottom = bottom[:n]

    n_wrong = 0
    for top_row, bottom_row in zip(top, bottom):
        for top_value, bottom_value in zip(top_row, bottom_row):
            if top_value != bottom_value:
                n_wrong += 1
                if n_wrong > 1:
                    return False

    return n_wrong == 1


def get_result(patterns, check_fn):
    result = 0
    for p in patterns:
        p_result = None
        for row_idx in range(len(p)):
            if check_fn(p, row_idx):
                p_result = 100 * row_idx
                break

        if p_result:
            result += p_result
            continue

        # transpose pattern to list of columns to use the same reflection function
        columns = []
        for c in range(len(p[0])):
            columns.append([row[c] for row in p])
        # check columns
        for column_idx in range(len(columns)):
            if check_fn(columns, column_idx):
                result += column_idx
                break
    return result


def part_1(patterns):
    t = time()
    print("Part 1:", get_result(patterns, is_reflected), f"(took {time() - t:.4f}s)")


def part_2(patterns):
    t = time()
    print(
        "Part 2:",
        get_result(patterns, reflection_if_smudge),
        f"(took {time() - t:.4f}s)",
    )


if __name__ == "__main__":
    with open("input.txt") as f:
        patterns = parse_data(f)
    part_1(patterns)
    part_2(patterns)
