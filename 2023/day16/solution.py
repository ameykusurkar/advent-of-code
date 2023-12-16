import numpy as np
from itertools import chain

# Multiples of i to rotate by
VERT = {
    '/': [-1],
    '.': [0],
    '|': [0],
    '-': [-1, 1],
    '\\': [1],
}

HORIZ = {
    '/': [1],
    '.': [0],
    '|': [-1, 1],
    '-': [0],
    '\\': [-1],
}

MOVES = {
  (-1,  0): VERT, # up
  ( 0, -1): HORIZ, # left
  ( 1,  0): VERT, # down
  ( 0,  1): HORIZ, # right
}

def rotate(point: tuple[int, int], n: int) -> tuple[int, int]:
    rotated = complex(point[1], -point[0]) * (1j ** n)
    return -int(rotated.imag), int(rotated.real)

def is_within(grid: np.ndarray, pos: tuple[int, int]) -> bool:
    target = grid.shape
    return 0 <= pos[0] < target[0] and 0 <= pos[1] < target[1]

def move(grid: np.ndarray, pos: tuple[int, int], dir: tuple[int, int]):
    outs = set()
    item = grid[pos]

    for i in MOVES[dir][item]:
        new_dir = rotate(dir, i)
        new_pos = (pos[0] + new_dir[0], pos[1] + new_dir[1])
        if is_within(grid, new_pos):
            outs.add((new_pos, new_dir))
    return outs

# yields (start_pos, start_dir)
def edges(grid):
    rows, cols = grid.shape
    return chain(
        [((i,        0), (0,  1)) for i in range(rows)], # left edge
        [((i, cols - 1), (0, -1)) for i in range(rows)], # right edge
        [((0,        j), (1,  0)) for j in range(cols)], # top edge
        [((rows - 1, j), (-1, 0)) for j in range(cols)], # bottom edge
    )

def energize(grid, start) -> int:
    beams = set([start])
    visited = set()
    while beams:
        pos, dir = beams.pop()
        visited.add((pos, dir))
        new = move(grid, pos, dir) - visited
        visited |= new
        beams |= new
    return len(set(p for p, _ in visited))

lines = open(0).read().split()
grid = np.array([list(l) for l in lines])

print(energize(grid, ((0, 0), (0, 1))))
print(max(energize(grid, start) for start in edges(grid)))
