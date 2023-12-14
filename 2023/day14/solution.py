import numpy as np

grid = np.array([list(l.strip()) for l in open(0).read().splitlines()])

rows, cols = grid.shape
while True:
    changed = False
    for i in reversed(range(1, rows)):
        for j in range(cols):
            if grid[i, j] == 'O' and grid[i-1, j] == '.':
                grid[i, j] = '.'
                grid[i-1, j] = 'O'
                changed = True
    if not changed:
        break

total = 0
for i in range(rows):
    for j in range(cols):
        if grid[i, j] == 'O':
            total += rows - i
print(total)
