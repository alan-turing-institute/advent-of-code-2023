from numpy import inf
import tqdm

import concurrent.futures


def parse_input(filename):
    with open(filename, 'r') as file:
        input_text = file.read()
    input_sections = input_text.split("\n\n")
    parsed_data = {
        'seeds': [section.split("\n")[0][7:].split(" ") for section in input_sections if section.split("\n")[0].startswith("seeds:")],
        'maps': {section.split("\n")[0][:-5]: [line.split(" ") for line in section.split("\n")[1:]] for section in input_sections if section.split("\n")[0].endswith(" map:")}
    }
    return parsed_data

def get_next_value(num, mapping):
        for rang in mapping:
            if int(rang[1]) <= num < int(rang[1])+int(rang[2]):
                num = int(rang[0]) + num - int(rang[1])
                return(num)
        return(num)


def part_one(seeds, mappings):

    lowest_result = inf

    seen_seeds = {}

    # make tqdm
    pg = tqdm.tqdm(total=len(seeds))
    for num in seeds:
        pg.update(1)
        start = num
        if start not in seen_seeds:
            for map in mappings:
                num = get_next_value(num, mappings[map])
            seen_seeds[start] = num
        if seen_seeds[start] < lowest_result:
            lowest_result = num
    return(lowest_result)


#############################################


def part_two_worker(num, mappings, seen_seeds):
    start = num
    if start not in seen_seeds:
        for map in mappings:
            num = get_next_value(num, mappings[map])
        seen_seeds[start] = num
    return num

def part_two(seed_params, mappings):
    seen_seeds = {}

    # yield seed values one by one - avoid storing a huge list in memory
    def generate_seeds(seed_params):
        for i in range(0, len(seed_params), 2):
            start = int(seed_params[i])
            length = int(seed_params[i+1])
            for j in range(length):
                yield start + j

    lowest_result = inf

    num_seeds = sum([int(seed_params[i + 1]) for i in range(0, len(seed_params), 2)])
    # make tqdm
    pg = tqdm.tqdm(total=num_seeds)

    with concurrent.futures.ProcessPoolExecutor() as executor:
        # Use a list to store futures
        futures = []

        for num in generate_seeds(seed_params):
            pg.update(1)

            # Submit tasks to the executor
            future = executor.submit(part_two_worker, num, mappings, seen_seeds)
            futures.append(future)

        # Wait for all tasks to complete
        concurrent.futures.wait(futures)

        # Get results from futures
        for future in futures:
            result = future.result()
            if result < lowest_result:
                lowest_result = result

    return lowest_result



if __name__ == '__main__':
    # data = parse_input("example.txt")
    data = parse_input("input.txt")
    # print(part_one([int(s) for s in data['seeds'][0]],  data['maps']))
    print(part_two([int(s) for s in data['seeds'][0]],  data['maps']))