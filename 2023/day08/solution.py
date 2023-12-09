from sys import stdin
from itertools import cycle, count
from math import lcm

dirs = cycle(next(stdin).strip())
next(stdin)

map = {}
for line in stdin:
    key, rest = line.split(" = ")
    left, right = rest.split(', ')
    map[key] = (left[1:4], right[:3])

points = [p for p in map.keys() if p[2] == 'A']

def cycle_length(p, map, dirs) -> int:
    for i in count(0):
        if p[2] == 'Z':
            return i
        d = next(dirs)
        pos = 0 if d == 'L' else 1
        p = map[p][pos]
    return -1

lengths = [cycle_length(p, map, dirs) for p in points]
print(lcm(*lengths))
