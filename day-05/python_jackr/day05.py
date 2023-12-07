import re
import time


def parse_data(file):
    sections = file.read().split("\n\n")
    seeds = re.findall(r"\d+", sections[0].split(": ")[1])
    seeds = [int(s) for s in seeds]

    maps = []
    for sec in sections[1:]:
        ranges = sec.split(":")[1].strip().splitlines()
        # a map is a list of ranges
        maps.append([])
        for r in ranges:
            # ranges are 3 values: (desination start ID, source start ID, length)
            digits = re.findall(r"\d+", r)
            maps[-1].append([int(d) for d in digits])
            # sort maps ascending by source start id (2nd value) - needed for part 2 logic
            maps[-1] = sorted(maps[-1], key=lambda x: x[1])
    return seeds, maps


# --------------------------------------
# PART 1
# --------------------------------------


def in_range(idx, start, size):
    return start <= idx < start + size


def get_destination_id(src_id, mapping):
    # source ID -> destination ID
    for m in mapping:
        dest_start, src_start, r_size = m
        if in_range(src_id, src_start, r_size):
            return dest_start + (src_id - src_start)
    return src_id  # destination_id = source_id if no match


def part_1(seeds, maps):
    t = time.time()
    min_loc = None
    for src_id in seeds:
        for mapping in maps:
            src_id = get_destination_id(src_id, mapping)
        if min_loc is None or src_id < min_loc:
            min_loc = src_id
    print(f"Part 1: {min_loc} (in {time.time() - t:.4f}s)")


# --------------------------------------
# PART 2
# --------------------------------------


def _get_single_destination_range(query_range, mapping):
    # (source start ID, length of range) -> list of (destination start ID, length of range)

    # extract query parameters [the source IDs we want to find]
    query_src_start, query_src_size = query_range
    query_src_end = query_src_start + query_src_size

    # match_ranges stores the destination ranges that match the query
    # (list of [(destination start id, range length)])
    match_ranges = []

    for m in mapping:  # (the ranges in mapping are ordered by src start id)
        # extract parameters for this map (which we'll check for overlaps with the query)
        map_dest_start, map_src_start, map_size = m
        map_src_end = map_src_start + map_size

        if query_src_start < map_src_start:
            # the query starts before the mapping, so map to same destination id as src
            # id (until the end of the query or the start of the map)
            match_ranges.append(
                [query_src_start, min([map_src_start, query_src_end]) - query_src_start]
            )
            if query_src_end <= map_src_start:
                # reached the end of the query so don't need to keep searching
                return match_ranges
            else:
                # move the start of the query (then progress to overlap logic below)
                query_src_start = map_src_start

        if in_range(query_src_start, map_src_start, map_size):
            # the (remaining) query overlaps with this mapping
            # compute the start ID of the destination range based on the src IDs
            query_dest_start = map_dest_start + (query_src_start - map_src_start)

            if query_src_end <= map_src_end:
                # all of the remaining query fits in this mapping, store the matching
                # destination range and then we've finished processing the query
                range_size = query_src_end - query_src_start
                match_ranges.append([query_dest_start, range_size])
                return match_ranges
            else:
                # part of the remaining query fits in this mapping, store the matching
                # destination range then continue and search the next mapping with
                # the remainder of the query
                range_size = map_src_end - query_src_start
                match_ranges.append([query_dest_start, range_size])
                query_src_start = map_src_end

    # the query extends above the max defined mapping, so the remaining destination ids
    # will match the src ids
    match_ranges.append([query_src_start, query_src_end - query_src_start])

    return match_ranges


def get_destination_ranges(src_ranges, mapping):
    # list of (source start ID, length of range) -> list of (destination start ID, length of range)
    all_ranges = [_get_single_destination_range(r, mapping) for r in src_ranges]
    # flatten list of lists
    return [r for query_ranges in all_ranges for r in query_ranges]


def part_2(seeds, maps):
    t = time.time()
    src_ranges = list(zip(seeds[0::2], seeds[1::2]))
    for mapping in maps:
        # work through each layer of the mapping (seed to soil to fertilizer etc.),
        # updating the new ID ranges that need to be searched each iteration
        src_ranges = get_destination_ranges(src_ranges, mapping)
    print(f"Part 2: {min(r[0] for r in src_ranges)} (in {time.time() - t:.4f}s)")


if __name__ == "__main__":
    t = time.time()
    with open("input.txt") as f:
        seeds, maps = parse_data(f)
    print(f"Parse in {time.time() - t:.4f}s")
    part_1(seeds, maps)
    part_2(seeds, maps)
    print(f"TOTAL TIME {time.time() - t:.4f}s")
