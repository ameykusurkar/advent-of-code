import numpy as np
from collections import defaultdict
from sys import stdin
from math import prod

D = '0123456789'

def numbers(grid: np.ndarray):
    rows, cols = grid.shape
    for i in range(1, rows):
        for j in range(1, cols):
            if grid[i, j] in D and grid[i, j-1] not in D:
                end_j = j + next(idx for (idx, n) in enumerate(grid[i, j:]) if n not in D)
                area = grid[i-1:i+2, j-1:end_j+1]
                val = int(''.join(grid[i, j:end_j]))
                yield(i, j, area, val)

def has_sym(area: np.ndarray) -> bool:
    not_syms = D + '.'
    return any(c for c in area.ravel() if c not in not_syms)

def find_gears(area: np.ndarray) -> list[tuple[int, int]]:
    rs, cs = area.shape
    gear_locations = []
    for i in range(0, rs):
        for j in range(0, cs):
            if area[i, j] == '*':
                gear_locations.append((i, j))
    return gear_locations

lines = stdin.read().split()
parsed = np.array([list(l) for l in lines])
grid = np.pad(parsed, (1, 1), constant_values='.')

part1 = sum(val for _, _, area, val in numbers(grid) if has_sym(area))
print(part1)

gears = defaultdict(list)
for i, j, area, val in numbers(grid):
    for (gi, gj) in find_gears(area):
        gears[(i + gi - 1, j + gj - 1)].append(val)

part2 = sum(prod(vals) for vals in gears.values() if len(vals) > 1)
print(part2)
