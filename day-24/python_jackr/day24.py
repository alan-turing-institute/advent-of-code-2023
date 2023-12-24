from dataclasses import dataclass
from time import time

from z3 import Int, Solver

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

    # get coordinates a and b trajectories cross
    # m_b*x + c_b = m_a*x + c_a
    if slope_b == slope_a:
        if intercept_a == intercept_b:
            raise ValueError(f"identical trajectories {a}, {b}")
        return False
    x = (intercept_a - intercept_b) / (slope_b - slope_a)
    y = slope_a * x + intercept_a

    # get times trajectories cross (don't need to cross at same time)
    t_a = (y - a.y) / a.vy
    t_b = (y - b.y) / b.vy

    # return coords if cross in future, else False
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


with open("input.txt") as f:
    STONES = parse_data(f)


def part_1():
    t = time()
    area = (200000000000000, 400000000000000)
    count = 0
    for idx, a in enumerate(STONES[:-1]):
        for b in STONES[(idx + 1) :]:
            inter_xy = intercept_xy(a, b)
            if inter_xy:
                x, y = inter_xy
                if area[0] <= x <= area[1] and area[0] <= y <= area[1]:
                    count += 1

    print("Part 1:", count, f"(took {time() - t:.4f}s)")


def part_2():
    """
    I wrote out the system of equation with pen and paper, then needed a hint for how
    to solve it where the answer seemd to be "chuck it at the z3 library" (the equations
    have interaction terms so not purely linear).

    Say the rock, r (with init coordinates xr,yr,zr,vxr,vyr,vzr), hits hailstone a
    (with init coordinates xa,ya,za,vxa,vya,vza), at time ta:
        - Position of r at ta: (xr + vxr*ta, yr + vyr*tb, zr + vzr*ta)
        - Position of a at ta: (xa + vxa*ta, ya + vya*ta, za + vza*ta)
    Set these equal to each other (so x,y,z coord are the same for a collision).

    We need to do this with 3 hailstones (a, b, c) to get 9 equations and 9 unknowns to
    solve.
    """
    t = time()

    a = STONES[0]
    b = STONES[1]
    c = STONES[2]

    xa, ya, za, vxa, vya, vza = a.x, a.y, a.z, a.vx, a.vy, a.vz
    xb, yb, zb, vxb, vyb, vzb = b.x, b.y, b.z, b.vx, b.vy, b.vz
    xc, yc, zc, vxc, vyc, vzc = c.x, c.y, c.z, c.vx, c.vy, c.vz

    # rock coordinates
    xr = Int("xr")
    vxr = Int("vxr")
    yr = Int("yr")
    vyr = Int("vyr")
    zr = Int("zr")
    vzr = Int("vzr")
    # time hit a, b, c
    ta = Int("ta")
    tb = Int("tb")
    tc = Int("tc")

    s = Solver()
    s.add(xr + vxr * ta == xa + vxa * ta)
    s.add(yr + vyr * ta == ya + vya * ta)
    s.add(zr + vzr * ta == za + vza * ta)

    s.add(xr + vxr * tb == xb + vxb * tb)
    s.add(yr + vyr * tb == yb + vyb * tb)
    s.add(zr + vzr * tb == zb + vzb * tb)

    s.add(xr + vxr * tc == xc + vxc * tc)
    s.add(yr + vyr * tc == yc + vyc * tc)
    s.add(zr + vzr * tc == zc + vzc * tc)

    print(s.check())
    result = s.model()

    print(
        "Part 2:",
        result[xr].as_long() + result[yr].as_long() + result[zr].as_long(),
        f"(took {time() - t:.4f}s)",
    )


if __name__ == "__main__":
    part_1()
    part_2()
    print(f"(overall {time() - start:.4f}s)")
