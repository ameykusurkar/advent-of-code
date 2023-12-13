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

def find_reflection(grid, old):
    for rows, reflect in reflections(grid):
        if reflect and (rows, 0) != old:
            return rows, 0
    for cols, reflect in reflections(grid.transpose()):
        if reflect and (0, cols) != old:
            return 0, cols
    return (-1, -1)

def find_reflection2(grid, old):
    rows, cols = grid.shape
    for i in range(rows):
        for j in range(cols):
            prev = grid[i, j]
            if old == '#':
                grid[i, j] = '.'
            else:
                grid[i, j] = '#'
            result = find_reflection(grid, old)
            if result != (-1, -1):
                return result
            grid[i, j] = prev
    raise Exception("what?!")

raw = open(0).read().split("\n\n")
grids = []
for r in raw:
    grids.append(np.array([list(line.strip()) for line in r.splitlines()]))

total1 = []
for grid in grids:
    rows, cols = find_reflection(grid, (-1, -1))
    total1.append((rows, cols))
print(sum(100 * rows + cols for rows, cols in total1))

total2 = 0
for grid, old in zip(grids, total1):
    rows, cols = find_reflection2(grid, old)
    total2 += 100 * rows + cols
print(total2)
