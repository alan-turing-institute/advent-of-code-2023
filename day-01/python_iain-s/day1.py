"""Solve the day's problem."""
import re


def one(lines: list[str]) -> int:
    digits = [re.findall(r"\d", line) for line in lines]
    first_and_last = [int(cal_val[0] + cal_val[-1]) for cal_val in digits]
    return sum(first_and_last)


def two(lines: list[str]) -> int:
    find_replace_dict = {
        "one": "1",
        "two": "2",
        "three": "3",
        "four": "4",
        "five": "5",
        "six": "6",
        "seven": "7",
        "eight": "8",
        "nine": "9",
    }

    def iterative_replace(a_line: str, a_dict: dict[str, str]) -> str:
        while True:
            a = []
            for key in a_dict.keys():
                if key in a_line:
                    a.append((a_line.find(key), key))

            if not a:
                return a_line
            a.sort(key=lambda x: x[0])
            a_line = a_line.replace(a[0][1], a_dict[a[0][1]])

    def func(a_line: str) -> str:
        new_line = ""
        while True:
            match = re.search(
                r"(\d|one|two|three|four|five|six|seven|eight|nine)", a_line
            )
            if match is None:
                return new_line
            else:
                new_line += match.group(1)
                a_line = a_line[match.start() + 1 :]

    result = []
    for line in lines:
        result.append(iterative_replace(func(line), find_replace_dict))

    return one(result)


def main() -> None:
    with open("./inputs/b/day1.txt", encoding="utf-8") as f:
        lines = [line.rstrip() for line in f]
    print("one:", one(lines))
    print("two:", two(lines))


if __name__ == "__main__":
    main()
