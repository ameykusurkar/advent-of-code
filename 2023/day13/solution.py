import numpy as np

def is_reflection(g: np.ndarray) -> bool:
    l = 0
    r = len(g) - 1
    while l < r:
        if not np.array_equal(g[l], g[r]):
            return False
        l += 1
        r -= 1
    return True

def reflections(grid):
    rows, _ = grid.shape
    for l, r in [(0, i) for i in range(2, rows, 2)] + [(i, rows) for i in range(1, rows-1, 2)]:
        yield((r+l)//2, is_reflection(grid[l:r]))

def find_reflection(grid):
    for rows, reflect in reflections(grid):
        if reflect:
            return rows, 0
    for cols, reflect in reflections(grid.transpose()):
        if reflect:
            return 0, cols
    raise Exception("no reflections!")

raw = open(0).read().split("\n\n")
grids = []
for r in raw:
    grids.append(np.array([list(line.strip()) for line in r.splitlines()]))

total = 0
for grid in grids:
    rows, cols = find_reflection(grid)
    total += cols + 100 * rows
print(total)
