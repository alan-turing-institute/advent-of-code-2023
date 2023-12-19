import math
from time import time

start = time()

with open("input.txt") as f:
    raw_workflows, parts = f.read().split("\n\n")

# parse workflows
# workflows is dict of name: list of steps
# steps are tuple (rule, outcome) where rule is the condition that must be satisfied to
# reach the outcome (or None if there are no conditions). Outcome is the name of
# another workflow, R to reject the part, or A to accept the part
raw_workflows = raw_workflows.splitlines()
workflows = {}
for idx, w in enumerate(raw_workflows):
    name, rest = w.split("{")
    steps = []
    for wflow in rest[:-1].split(","):
        rule_outcome = wflow.split(":")
        if len(rule_outcome) == 1:
            rule = None
            outcome = rule_outcome[0]
        else:
            rule, outcome = rule_outcome

        steps.append((rule, outcome))
    workflows[name] = steps

# parse parts
# parts will be list of [x,m,a,s] values
parts = parts.splitlines()
for idx, p in enumerate(parts):
    parts[idx] = []
    p = p[1:-1]  # strip { }
    categories = p.split(",")
    for category in p.split(","):
        kind = category[0]
        val = int(category[2:])
        parts[idx].append(val)


# ---------------------------------
# Part 1
# ---------------------------------
def run_workflow(part, workflow):
    x, m, a, s = part  # used by eval below
    for step in workflow:
        rule, outcome = step
        if rule is None or eval(rule):  # rule is a string like 'x > 500'
            # we passed the condition for this workflow step
            if outcome == "A":
                # accept so return this part's value
                return sum(part)
            elif outcome == "R":
                # rejected so this part has no value
                return 0
            else:
                # don't know the outcome yet, the outcome specifies the next workflow
                # to evaluate
                return run_workflow(part, workflows[outcome])


result = 0
for p in parts:
    result += run_workflow(p, workflows["in"])
print("Part 1:", result)

# ---------------------------------
# Part 2
# ---------------------------------

ranges = []

# states are [workflow, step ID, xrange, mrange, arange, srange]
# start with everything being valid and propagate/reduce ranges as we go
queue = [("in", 0, (1, 4000), (1, 4000), (1, 4000), (1, 4000))]
categories = {"x": 2, "m": 3, "a": 4, "s": 5}  # mapping to id of range for each state

# keep searching for valid states until we've exhausted all possible ranges
# the below is an ugly mess of intersecting ranges and upating states, sorry
while queue:
    state = queue.pop()
    workflow = workflows[state[0]]
    step = state[1]
    rule, outcome = workflow[step]

    # If there's no rule we can immediately check the outcome and move on
    if rule is None:
        if outcome == "A":
            ranges.append(state[2:])
        elif outcome != "R":
            # move to start of outcome workflow (with the valid ranges in the state unchanged)
            queue.append((outcome, 0, *state[2:]))
        continue

    # Got a condition to check (uh oh)
    cat = categories[rule[0]]  # category (x, m, a, or s mapped to an idx)
    op = rule[1]  # > or < operation
    value = int(rule[2:])  # the value to compare the category value to
    cat_range = state[cat]  # the category range the operation acts on

    # Find the part of the range that passes the rule
    if op == "<":
        if cat_range[0] < value:
            passed_range = (cat_range[0], min(value - 1, cat_range[1]))
        else:
            passed_range = None
    elif op == ">":
        if cat_range[1] > value:
            passed_range = (max(value + 1, cat_range[0]), cat_range[1])
        else:
            passed_range = None

    # If there is a part of the range that passes, build the new state that passes
    # (which contains both the ranges + info about the workflow and workflow step)
    if passed_range is not None:
        # Either side of the passed range we now have two failed ranges
        failed_ranges = [
            (cat_range[0], passed_range[0] - 1),
            (passed_range[1] + 1, cat_range[1]),
        ]

        # build new ranges using updated category range + all others unchanged
        if cat + 1 < len(state):
            passed_state = (
                outcome,
                0,
                *state[2:cat],
                passed_range,
                *state[cat + 1 :],
            )
        else:
            passed_state = (outcome, 0, *state[2:cat], passed_range)

        # check how to continue
        if outcome == "A":
            # this is a successful state so add it to the list of good ranges
            ranges.append(passed_state[2:])
        elif outcome != "R":
            # this workflow didn't fail but there is more to process - add the next
            # state to the queue
            queue.append(passed_state)
    else:
        # if nothing passed the whole original range is a failed_range
        failed_ranges = [cat_range]

    # check failed_ranges are valid and buil them into full states if so
    # failed states will move on to the next step in the current workflow
    failed_ranges = [r for r in failed_ranges if r[1] > r[0]]
    for fr in failed_ranges:
        if cat + 1 < len(state):
            failed_state = (
                state[0],
                step + 1,
                *state[2:cat],
                fr,
                *state[cat + 1 :],
            )
        else:
            failed_state = (state[0], step + 1, *state[2:cat], fr)
        queue.append(failed_state)


combinations = 0
for r in ranges:
    combinations += math.prod(c[1] - c[0] + 1 for c in r)

print("Part 2:", combinations)
print(f"(took {time() - start:.4f}s)")
