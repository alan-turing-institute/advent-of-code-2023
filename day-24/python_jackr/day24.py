from dataclasses import dataclass
from time import time

start = time()


@dataclass
class Stone:
    x: int
    y: int
    z: int
    vx: int
    vy: int
    vz: int

    def pos(self, t):
        return (self.x + t * self.vx, self.y + t * self.vy, self.z + t * self.vz)

    def fit_xy(self):
        slope = self.vy / self.vx
        intercept = self.y - slope * self.x

        return slope, intercept


def intercept_xy(a: Stone, b: Stone):
    slope_a, intercept_a = a.fit_xy()
    slope_b, intercept_b = b.fit_xy()

    """
    by = ay
    bx = ax
    t > 0
    """

    # m_b*x + c_b = m_a*x + c_a
    if slope_b == slope_a:
        if intercept_a == intercept_b:
            raise ValueError(f"identical trajectories {a}, {b}")
        return False
    x = (intercept_a - intercept_b) / (slope_b - slope_a)
    y = slope_a * x + intercept_a
    # print(y, slope_b * x + intercept_b)
    # y = y0 + t * vy
    t_a = (y - a.y) / a.vy
    t_b = (y - b.y) / b.vy
    # print(t, (y - b.y) / b.vy)

    # if b.vx != a.vx:
    #     t_x = (a.x - b.x) / (b.vx - a.vx)
    # else:
    #     t_x = 1  # FIXME: /0 fudge
    # if b.vy != a.vy:
    #     t_y = (a.y - b.y) / (b.vy - a.vy)
    # else:
    #     t_y = 1  # FIXME: /0 fudge

    # print("tx ty", t_x, t_y)

    return (x, y) if t_a > 0 and t_b > 0 else False


def parse_data(file):
    stones = []
    for line in file.read().splitlines():
        pos, vel = line.split(" @ ")
        pos = pos.split(", ")
        vel = vel.split(", ")
        stones.append(
            Stone(
                int(pos[0]),
                int(pos[1]),
                int(pos[2]),
                int(vel[0]),
                int(vel[1]),
                int(vel[2]),
            )
        )
    return stones


with open(
    "/Users/jroberts/repos/advent-of-code-2023/day-24/python_jackr/input.txt"
) as f:
    STONES = parse_data(f)


def part_1():
    import matplotlib.pyplot as plt
    import numpy as np

    count = 0
    for idx, a in enumerate(STONES[:-1]):
        for b in STONES[(idx + 1) :]:
            # am, ac = a.fit_xy()
            # axs = []
            # ays = []
            # a_fitys = []
            # for t in range(1000):
            #     x, y, z = a.pos(t)
            #     axs.append(x)
            #     ays.append(y)
            #     a_fitys.append(x * am + ac)
            # plt.plot(axs, ays, "bo")
            # plt.plot(axs, a_fitys, "b")

            # bm, bc = b.fit_xy()
            # bxs = []
            # bys = []
            # b_fitys = []
            # for t in range(100):
            #     x, y, z = b.pos(t)
            #     bxs.append(x)
            #     bys.append(y)
            #     b_fitys.append(x * bm + bc)
            # plt.plot(bxs, bys, "ro")
            # plt.plot(bxs, b_fitys, "r")

            # inter_xy = intercept_xy(a, b)
            # if inter_xy is True:
            #     count += 1
            # elif inter_xy is not False:
            #     plt.plot([x], [y], "X", markersize=20)
            #     x, y = inter_xy
            #     if 7 <= x <= 27 and 7 <= y <= 27:
            #         count += 1

            # plt.show()
            area = (200000000000000, 400000000000000)
            inter_xy = intercept_xy(a, b)
            if inter_xy:
                x, y = inter_xy
                if area[0] <= x <= area[1] and area[0] <= y <= area[1]:
                    # print("true", a, b, (x, y))
                    count += 1
                else:
                    # print("outside", a, b, (x, y))
                    ...
            else:
                # print("false", a, b)
                ...

    t = time()

    print("Part 1:", count, f"(took {time() - t:.4f}s)")


def part_2():
    t = time()

    print("Part 2:", f"(took {time() - t:.4f}s)")


if __name__ == "__main__":
    part_1()
    part_2()
    print(f"(overall {time() - start:.4f}s)")
