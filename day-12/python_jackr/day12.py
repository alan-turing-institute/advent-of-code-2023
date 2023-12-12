from functools import cache
from time import time


def parse_data(file) -> (list[list[str]], list[int]):
    lines = file.read().splitlines()
    records = []
    for line in lines:
        springs, groups = line.split()
        groups = tuple(int(g) for g in groups.split(","))
        records.append((springs, groups))
    return records


@cache
def count_arrangements(springs, required_groups, curr_group_size):
    # End condition: Reached the end of the springs string
    if len(springs) == 0:
        if len(required_groups) == 0:
            # correct if no more groups required to match record and no group currently
            # in progress
            return curr_group_size == 0
        elif len(required_groups) == 1:
            # correct if one more group required and group currently in progress is of
            # that size
            return curr_group_size == required_groups[0]
        else:
            # didn't fill all the required groups
            return False

    # End condition: no valid solutions if we need no more groups but have # remaining
    if len(required_groups) == 0 and "#" in springs:
        return False

    # Continue the search
    count = 0
    if springs[0] == ".":
        if curr_group_size > 0:
            # this is the end of a group
            if len(required_groups) > 0 and curr_group_size == required_groups[0]:
                # the created group was of the right size, so continue the search to
                # look for the next group in the remaining springs record
                count += count_arrangements(springs[1:], required_groups[1:], 0)
            # else:
            #   group created was the wrong size so no correct solutions to count

        else:
            # keep looking for the start of the next group
            count += count_arrangements(springs[1:], required_groups, 0)

    elif springs[0] == "#":
        # increases the size of the current group by one
        count += count_arrangements(springs[1:], required_groups, curr_group_size + 1)

    else:
        # symbol is ?, count the valid arrangements for the two possibilities
        count += count_arrangements("#" + springs[1:], required_groups, curr_group_size)
        count += count_arrangements("." + springs[1:], required_groups, curr_group_size)

    return count


def part_1(records):
    t = time()
    print(
        "Part 1:",
        sum(count_arrangements(r[0], r[1], 0) for r in records),
        f"(took {time() - t:.4f}s)",
    )


def part_2(records):
    t = time()
    print(
        "Part 2:",
        sum(count_arrangements("?".join([r[0]] * 5), r[1] * 5, 0) for r in records),
        f"(took {time() - t:.4f}s)",
    )


if __name__ == "__main__":
    with open("input.txt") as f:
        records = parse_data(f)
    part_1(records)
    part_2(records)
