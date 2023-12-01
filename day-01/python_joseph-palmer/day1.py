import numpy as np

def part1(data):
    vals = [None] * len(data)
    for i, line in enumerate(data):
        numerics = [str(x) for x in line if x.isdigit()]
        vals[i] = int(numerics[0]+numerics[-1])
    return sum(vals)
        

def part2(data):
    valdict = {
        "one" : 1, "two" : 2, "three" : 3, "four" : 4, "five" : 5,
        "six" : 6, "seven" : 7, "eight" : 8, "nine" : 9
    }
    newlines = []
    for line in data:
        for key in valdict.keys():
            line = line.replace(key, key[0]+str(valdict[key])+key[-1])
        newlines.append(line)
    return part1(newlines)



testdata = np.genfromtxt('day1_testdata.txt', delimiter='\n', dtype=str)
testdatap2 = np.genfromtxt('day1_testdata_p2.txt', delimiter='\n', dtype=str)
actualdata = np.genfromtxt('day1_data.txt', delimiter='\n', dtype=str)

assert part1(testdata) == 142
assert part2(testdatap2) == 281

print(
    f"part 1: {part1(actualdata)}\npart 2: {part2(actualdata)}"
)