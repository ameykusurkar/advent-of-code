import numpy as np

def north(grid):
    for j in range(grid.shape[1]):
        slide(grid[:, j])

def south(grid):
    grid = np.flip(grid, 0)
    north(grid)
    grid = np.flip(grid, 0)

def west(grid):
    for i in range(grid.shape[0]):
        slide(grid[i])

def east(grid):
    grid = np.flip(grid, 1)
    west(grid)
    grid = np.flip(grid, 1)

def slide(arr):
    items = len(arr)
    start, end = 0, 0
    while start < items - 1:
        while arr[start] != '.' and start < items - 1:
            start += 1
        if start == items - 1:
            return
        # start points to '.'
        end = start + 1
        while end < items and arr[end] == '.':
            end += 1
        if end == items:
            return
        if arr[end] == '#':
            start = end + 1
            continue
        o_start = end
        # o_start points to first 'O'
        while end < items and arr[end] == 'O':
            end += 1
        # end points to first non-'O'
        for x in range(o_start, end):
            arr[x] = '.'
        num_os = end - o_start
        new_start = start + num_os
        while start < new_start:
            arr[start] = 'O'
            start += 1

def count(grid):
    rows, cols = grid.shape
    total = 0
    for i in range(rows):
        for j in range(cols):
            if grid[i, j] == 'O':
                total += rows - i
    return total

def serial(grid):
    return "".join(str(x) for x in grid)

def cycle(grid):
    north(grid)
    west(grid)
    south(grid)
    east(grid)

grid = np.array([list(l.strip()) for l in open(0).read().splitlines()])

#### part 1
# north(grid)
# print(count(grid))

#### part 2
curr = 0
seen = {}
seen[serial(grid)] = curr

while True:
    cycle(grid)
    curr += 1
    s = serial(grid)
    if s in seen:
        break
    seen[s] = curr

s = seen[serial(grid)]
cycle_length = curr - s
offset = (1_000_000_000 - s) % cycle_length

for _ in range(offset):
    cycle(grid)
print(count(grid))
