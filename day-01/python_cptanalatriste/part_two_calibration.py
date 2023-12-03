import part_one
from typing import Optional


def left_to_right(line: str, words_to_digits: dict[str, str]) -> Optional[str]:
    first_index: int = 0
    second_index: int = 1

    while second_index <= len(line):
        if line[first_index].isdigit():
            return line[first_index]

        current_word: str = line[first_index:second_index]
        if current_word in words_to_digits.keys():
            return words_to_digits[current_word]

        word_in_progress: bool = False
        for number in words_to_digits.keys():
            if number.startswith(current_word):
                word_in_progress = True
                break

        if word_in_progress:
            second_index += 1
            continue
        else:
            first_index += 1
            second_index = first_index + 1

    return None


def get_calibration_value(line: str) -> int:
    words_to_digits: dict[str, str] = {
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
    first_digit: Optional[str] = left_to_right(line, words_to_digits)

    reversed_words_to_digits: dict = {
        key[::-1]: value for key, value in words_to_digits.items()
    }
    second_digit: Optional[str] = left_to_right(line[::-1], reversed_words_to_digits)

    if first_digit is None or second_digit is None:
        raise Exception(
            f"Could not process line {line}. {first_digit=} {second_digit=}"
        )
    return int(first_digit + second_digit)


if __name__ == "__main__":
    part_one.main(parsing_function=get_calibration_value)
