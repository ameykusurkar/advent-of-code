import numpy as np
from time import time

def north(grid, j):
    start = time()
    rows, _ = grid.shape
    start, end = 0, 0
    while start < rows - 1:
        while grid[start, j] != '.' and start < rows - 2:
            start += 1
        if start == rows - 1:
            return
        # start points to '.'
        end = start + 1
        while end < rows and grid[end, j] == '.' and end < rows:
            end += 1
        if end == rows:
            return
        if grid[end, j] == '#':
            start = end + 1
            continue
        o_start = end
        # o_start points to first 'O'
        while end < rows and grid[end, j] == 'O':
            end += 1
        # end points to first non-'O'
        for x in range(o_start, end):
            grid[x, j] = '.'
        num_os = end - o_start
        new_start = start + num_os
        while start < new_start:
            grid[start, j] = 'O'
            start += 1

def count(grid):
    rows, cols = grid.shape
    total = 0
    for i in range(rows):
        for j in range(cols):
            if grid[i, j] == 'O':
                total += rows - i
    return total

grid = np.array([list(l.strip()) for l in open(0).read().splitlines()])
s = time()
for j in range(grid.shape[1]):
    north(grid, j)
print(time() - s)

print(count(grid))
