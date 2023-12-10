from sys import stdin
from math import ceil
import numpy as np

MOVES = {
  # Arranged based on multiples of i to rotate by
  #          -i    0    i  
  (-1,  0): ['F', '|', '7'], # up
  ( 1,  0): ['J', '|', 'L'], # down
  ( 0, -1): ['L', '-', 'F'], # left
  ( 0,  1): ['7', '-', 'J'], # right
}

def rotate(point: tuple[int, int], n: int) -> tuple[int, int]:
    rotated = complex(point[1], -point[0]) * (1j ** n)
    return -int(rotated.imag), int(rotated.real)

def is_within(target: tuple[int, int], pos: tuple[int, int]) -> bool:
    ti, tj = target
    pi, pj = pos
    return 0 <= pi < ti and 0 <= pj < tj

def find_start_dir(grid, pos) -> tuple[int, int]:
    for dir in MOVES.keys():
        next = move(grid, pos, dir)
        if next:
            return dir
    raise Exception(f"No valid start dir for {pos}")

def move(grid: np.ndarray, pos: tuple[int, int], dir: tuple[int, int]):
    new_pos = (pos[0] + dir[0], pos[1] + dir[1])
    if not is_within(grid.shape, new_pos):
        return None
    pipe = grid[new_pos] 
    if pipe == "S":
        return "loop"
    options = MOVES[dir]
    try:
        rotations = options.index(pipe) - 1
    except:
        return None
    new_dir = rotate(dir, rotations)
    return new_pos, new_dir

lines = stdin.read().split()
grid = np.array([list(l) for l in lines])

startI, startJ = np.where(grid == 'S')
start_pos = startI[0], startJ[0]
start_dir = find_start_dir(grid, start_pos)

current_pos, current_dir = start_pos, start_dir
count = 0
while True:
    count += 1
    match move(grid, current_pos, current_dir):
        case None:
            raise Exception("What happened")
        case "loop":
            break
        case next:
            current_pos, current_dir = next

print(ceil(count/2))
