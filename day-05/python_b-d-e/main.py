from numpy import inf

def parse_input(filename):
    with open(filename, 'r') as file:
        input_text = file.read()
    input_sections = input_text.split("\n\n")
    parsed_data = {
        'seeds': [section.split("\n")[0][7:].split(" ") for section in input_sections if section.split("\n")[0].startswith("seeds:")],
        'maps': {section.split("\n")[0][:-5]: [line.split(" ") for line in section.split("\n")[1:]] for section in input_sections if section.split("\n")[0].endswith(" map:")}
    }
    return parsed_data

def part_one(seeds, mappings):

    def get_next_value(num, mapping):
        for rang in mapping:
            if int(rang[1]) <= num < int(rang[1])+int(rang[2]):
                num = int(rang[0]) + num - int(rang[1])
                return(num)
        return(num)

    lowest_result = inf

    for num in seeds:
        for map in mappings:
            num = get_next_value(num, mappings[map])
        if num < lowest_result:
            lowest_result = num
    return(lowest_result)


data = parse_input("input.txt")
print(part_one([int(s) for s in data['seeds'][0]],  data['maps']))