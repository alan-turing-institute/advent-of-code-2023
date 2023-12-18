"""Solve the day's problem."""


def find_reflections(pattern: list[str]) -> list[tuple[str, int]]:
    """Look for horizontal and vertical reflections."""
    reflections = []
    for the_pattern, direction in (
        (pattern, "h"),
        (["".join(x) for x in zip(*pattern)], "v"),
    ):
        for i in range(1, len(the_pattern)):
            comparing = min(i, len(the_pattern) - i)
            if the_pattern[i - comparing : i] == list(
                reversed(the_pattern[i : i + comparing])
            ):
                reflections.append((direction, i))

    return reflections


def get_patterns(lines: list[str]) -> tuple[list[str], ...]:
    patterns = []
    pattern = []
    for line in lines:
        if line == "":
            patterns.append(pattern)
            pattern = []
        else:
            pattern.append(line)
    patterns.append(pattern)
    return tuple(patterns)


def one(lines: list[str]) -> int:
    reflections = tuple([find_reflections(pattern) for pattern in get_patterns(lines)])
    result = [x[1] * (1 if x[0] == "v" else 100) for x in [y[0] for y in reflections]]
    return sum(result)


def two(lines: list[str]) -> int:
    patterns = get_patterns(lines)
    reflections = []
    for pattern in patterns:
        reflections.append(account_for_smudge(pattern))

    result = [x[1] * (1 if x[0] == "v" else 100) for x in reflections]
    return sum(result)


def account_for_smudge(pattern: list[str]) -> tuple[str, int]:
    original = find_reflections(pattern)
    assert len(original) == 1

    for j in range(len(pattern)):
        for k in range(len(pattern[0])):
            # swap . for #
            pattern[j] = (
                pattern[j][:k]
                + ("." if pattern[j][k] == "#" else "#")
                + pattern[j][k + 1 :]
            )
            reflections = find_reflections(pattern)
            if len(reflections) == 1:
                if reflections[0] != original[0]:
                    return reflections[0]
            elif len(reflections) == 2:
                return (
                    reflections[0] if reflections[1] == original[0] else reflections[1]
                )
            elif len(reflections) > 2:
                raise Exception("Too many reflections")
            pattern[j] = (
                pattern[j][:k]
                + ("." if pattern[j][k] == "#" else "#")
                + pattern[j][k + 1 :]
            )

    raise Exception("No smudge found")


def main() -> None:
    with open("./inputs/b/day13.txt", encoding="utf-8") as f:
        lines = [line.rstrip() for line in f]
    print("one:", one(lines))

    with open("./inputs/b/day13.txt", encoding="utf-8") as f:
        lines = [line.rstrip() for line in f]
    print("two:", two(lines))


if __name__ == "__main__":
    main()
