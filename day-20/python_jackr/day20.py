from copy import deepcopy
from dataclasses import dataclass
from time import time

start = time()

# ---------------------------------
# Parsing
# ---------------------------------


@dataclass
class Pulse:
    src: str
    dest: str
    high: bool


@dataclass
class FlipFlop:
    name: str
    dests: tuple[str]
    on: bool = False

    def receive(self, pulse) -> list[Pulse]:
        if not pulse.high:
            self.on = not self.on
            return [Pulse(src=self.name, dest=d, high=self.on) for d in self.dests]
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
            return [Pulse(src=self.name, dest=d, high=False) for d in self.dests]
        return [Pulse(src=self.name, dest=d, high=True) for d in self.dests]


@dataclass
class Broadcast:
    name: str
    dests: tuple[str]

    def receive(self, pulse) -> list[Pulse]:
        return [Pulse(src=self.name, dest=d, high=pulse.high) for d in self.dests]


@dataclass
class Button:
    name: str = "button"
    dests: tuple[str] = ("broadcaster",)

    def push(self) -> list[Pulse]:
        return [Pulse(src=self.name, dest=d, high=False) for d in self.dests]


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


def part_2(modules):
    t = time()
    fz_states = set()
    fz_states.add(
        tuple(modules["fz"].memory.values()) + tuple(modules["tf"].memory.values())
    )
    n = 0
    while n < 100000:
        push(modules)
        n += 1
        new_state = tuple(modules["fz"].memory.values()) + tuple(
            modules["tf"].memory.values()
        )
        fz_states.add(new_state)
    print(fz_states)
    print(len(fz_states))
    print("=== n", n, "===")
    # n = 0
    # while not modules["rx"].received_low:
    #     push(modules)
    #     n += 1
    #     if n % 100000 == 0:
    #         print(n)
    #         print(modules)
    #         print("---")

    print("Part 2:", n, f"(took {time() - t:.4f}s)")


def plot(modules):
    """
    Visualise connections to get an idea for how the problem might simplify.
    From this I noticed (for my input):
    - rx input depends on ql only
    - ql depends on ss, fz, mf, fh
    - ss, fz, mf, fh are independent of each other
    So something about checking for repeating loops in the state of ss, fz, mf, fh
    and then lowest common multiple?
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
    with open("input.txt") as f:
        modules = parse_data(f)
    # plot(modules)
    part_1(deepcopy(modules))
    part_2(deepcopy(modules))
