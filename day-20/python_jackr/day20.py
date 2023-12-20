import heapq
import math
from copy import deepcopy
from dataclasses import dataclass
from time import time

start = time()

# ---------------------------------
# Parsing
# ---------------------------------


@dataclass(frozen=True)  # frozen=True makes instances hashable
class Pulse:
    src: str
    dest: str
    high: bool
    time: int

    def __lt__(self, other):
        return self.time < other.time


@dataclass
class FlipFlop:
    name: str
    dests: tuple[str]
    on: bool = False

    def receive(self, pulse) -> list[Pulse]:
        if not pulse.high:
            self.on = not self.on
            return [
                Pulse(src=self.name, dest=d, high=self.on, time=pulse.time + 1)
                for d in self.dests
            ]
        return []


@dataclass
class Conjunction:
    name: str
    dests: tuple[str]
    srcs: tuple[str]

    def __post_init__(self):
        self.memory: dict[bool] = {s: False for s in self.srcs}

    def receive(self, pulse) -> list[Pulse]:
        self.memory[pulse.src] = pulse.high
        if all(self.memory[s] for s in self.srcs):
            return [
                Pulse(src=self.name, dest=d, high=False, time=pulse.time + 1)
                for d in self.dests
            ]
        return [
            Pulse(src=self.name, dest=d, high=True, time=pulse.time + 1)
            for d in self.dests
        ]


@dataclass
class Broadcast:
    name: str
    dests: tuple[str]

    def receive(self, pulse) -> list[Pulse]:
        return [
            Pulse(src=self.name, dest=d, high=pulse.high, time=pulse.time + 1)
            for d in self.dests
        ]


@dataclass
class Button:
    name: str = "button"
    dests: tuple[str] = ("broadcaster",)

    def push(self) -> list[Pulse]:
        return [Pulse(src=self.name, dest=d, high=False, time=0) for d in self.dests]


@dataclass
class Stop:  # dummy module that receives pulses and does nothing
    name: str
    dests: tuple[str] = ()
    received_low: bool = False

    def receive(self, pulse) -> list[Pulse]:
        if not pulse.high:
            self.received_low = True
        return []


def parse_data(f):
    modules = {}
    conj = []
    names = set()
    for line in f.read().splitlines():
        src, dests = line.split(" -> ")

        dests = tuple(dests.split(", "))
        [names.add(d) for d in dests]

        if src == "broadcaster":
            names.add(src)
            modules[src] = Broadcast(src, dests)
        elif src[0] == "%":
            names.add(src[1:])
            modules[src[1:]] = FlipFlop(src[1:], dests)
        elif src[0] == "&":
            names.add(src[1:])
            # need to fill srcs later after all modules initialised so keep track of their names
            conj.append(src[1:])
            modules[src[1:]] = Conjunction(src[1:], dests, srcs=())

    # some modules can be specified as destinations only so won't be created above,
    # create them as 'Stop' nodes that do nothing when receiving a pulse
    for n in names:
        if n not in modules:
            modules[n] = Stop(n)

    modules["button"] = Button()

    for c in conj:
        srcs = []
        for m in modules.values():
            if c in m.dests:
                srcs.append(m.name)
        modules[c] = Conjunction(modules[c].name, modules[c].dests, tuple(srcs))
    return modules


# ---------------------------------
# Part 1
# ---------------------------------


def push(modules):
    pulses = modules["button"].push()
    n_low = 0
    n_high = 0
    while pulses:
        new_pulses = []
        for p in pulses:
            if p.high:
                n_high += 1
            else:
                n_low += 1
            new_pulses += modules[p.dest].receive(p)
        pulses = new_pulses
    return n_low, n_high


def part_1(modules):
    t = time()
    n_low = 0
    n_high = 0
    for _ in range(1000):
        l, h = push(modules)
        n_low += l
        n_high += h
    print("Part 1:", n_low * n_high, f"(took {time() - t:.4f}s)")


# ---------------------------------
# Part 2
# ---------------------------------


def find_loop(src, dest, high, modules):
    # returns the first two times (button pushes and time step within button push)
    # a high/low (depending on value of high) pulse is sent from src to dest
    found = []
    n = 0
    while len(found) < 2:
        n += 1
        pulses = modules["button"].push()
        while pulses:
            p = heapq.heappop(pulses)

            for new in modules[p.dest].receive(p):
                if new.src == src and new.dest == dest and new.high == high:
                    found.append((n, new.time))
                heapq.heappush(pulses, new)

    return found


def part_2(modules):
    t = time()

    # check when modules of interest send high signals to ql (which would then send
    # a low signal of all those are true)
    # see the docstring of the plot function for why these modules are the ones of
    # interest. I assume there's a neat way of automating the detection of these loops
    # but the visual/manual approach worked for me!
    check_modules = ["ss", "fz", "mf", "fh"]
    dest = "ql"
    loops = {}
    for cm in check_modules:
        loops[cm] = find_loop(cm, dest, True, deepcopy(modules))
    print(loops)
    # here I manually checked two things:
    #  - all the loops states are at the same internal time step within a button press
    #  - all the loops started at zero (they don't have an offset at the beginning
    #    before looping). They do so lowest common multiple of the first time the loop
    #    state appears is fine.
    print(
        "Part 2:",
        math.lcm(*[l[0][0] for l in loops.values()]),
        f"(took {time() - t:.4f}s)",
    )


def plot(modules):
    """
    Visualise connections to get an idea for how the problem might simplify.
    From this I noticed (for my input):
    - rx input depends on ql only
    - ql depends on ss, fz, mf, fh
    - ss, fz, mf, fh are independent of each other (in separated loops with other modules)
    - ql will send low to rx when the last pulse sent to it by ss, fz, mf, and fh were
      all high
    So check for repeating loops when ss, fz, mf, fh send high, then lowest common
    term in all those series will be the first time rx receives low.

    I've commited the saved visualisation as graph.pdf in the repo.
    """
    import networkx as nx

    colors = {
        "FlipFlop": "green",
        "Conjunction": "yellow",
        "Broadcast": "red",
        "Button": "red",
        "Stop": "red",
    }
    G = nx.DiGraph()
    for _, m in modules.items():
        G.add_node(m.name, fillcolor=colors[type(m).__name__], style="filled")
    for m in modules.values():
        for d in m.dests:
            G.add_edge(m.name, d)

    A = nx.drawing.nx_agraph.to_agraph(G)
    A.layout(prog="dot")
    A.draw("graph.pdf")


if __name__ == "__main__":
    with open(
        "/Users/jroberts/repos/advent-of-code-2023/day-20/python_jackr/input.txt"
    ) as f:
        modules = parse_data(f)
    # plot(modules) - need networkx, pygraphviz and graphviz installed to run this
    part_1(deepcopy(modules))
    part_2(deepcopy(modules))
