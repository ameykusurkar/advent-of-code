import numpy as np
from itertools import combinations

def count(locs, xBlanks, yBlanks, expand):
    total = 0
    for p1, p2 in combinations(locs, 2):
        y1, x1 = p1
        y2, x2 = p2
        yDiff = abs(y1 - y2)
        xDiff = abs(x1 - x2)
        for b in xBlanks:
            if min(x1, x2) < b < max(x1, x2):
                xDiff += expand - 1
        for b in yBlanks:
            if min(y1, y2) < b < max(y1, y2):
                yDiff += expand - 1
        total += xDiff + yDiff
    return total

grid = [list(line.strip()) for line in open(0)]
grid = np.array(grid)
rows, cols = grid.shape

xBlanks = []
for i in range(cols):
    if all(x == '.' for x in grid[:, i]):
        xBlanks.append(i)

yBlanks = []
for i in range(rows):
    if all(x == '.' for x in grid[i]):
        yBlanks.append(i)

locs = list(zip(*np.where(grid == "#")))

print(count(locs, xBlanks, yBlanks, 2))
print(count(locs, xBlanks, yBlanks, 1_000_000))
