from math import ceil
import numpy as np

MOVES = {
  # Arranged based on multiples of i to rotate by
  #          -i    0    i  
  (-1,  0): ['F', '|', '7'], # up
  ( 0, -1): ['L', '-', 'F'], # left
  ( 1,  0): ['J', '|', 'L'], # down
  ( 0,  1): ['7', '-', 'J'], # right
}

OFFSETS = set([
              (-1, 0),
    ( 0, -1),          ( 0, 1),
              ( 1, 0),
])

def rotate(point: tuple[int, int], n: int) -> tuple[int, int]:
    rotated = complex(point[1], -point[0]) * (1j ** n)
    return -int(rotated.imag), int(rotated.real)

def is_within(grid: np.ndarray, pos: tuple[int, int]) -> bool:
    target = grid.shape
    return 0 <= pos[0] < target[0] and 0 <= pos[1] < target[1]

def find_start_dir(grid, pos) -> tuple[int, int]:
    for dir in MOVES.keys():
        next = move(grid, pos, dir)
        if next:
            return dir
    raise Exception(f"No valid start dir for {pos}")

def move(grid: np.ndarray, pos: tuple[int, int], dir: tuple[int, int]):
    new_pos = (pos[0] + dir[0], pos[1] + dir[1])
    if not is_within(grid, new_pos):
        return None
    pipe = grid[new_pos] 
    if pipe == "S":
        return "loop"
    try:
        i = MOVES[dir].index(pipe) - 1
    except:
        return None
    new_dir = rotate(dir, i)
    return new_pos, new_dir

def side(grid: np.ndarray, pos: tuple[int, int], dir: tuple[int, int], rot):
    new_dir = rotate(dir, rot)
    new_pos = (pos[0] + new_dir[0], pos[1] + new_dir[1])
    if not is_within(grid, new_pos):
        return None
    return new_pos, grid[new_pos]

def floodfill(grid, to_visit: set[tuple[int, int]], main_loop) -> set[tuple[int, int]]:
    visited = set()
    while to_visit:
        pos = to_visit.pop()
        if pos in visited:
            continue
        visited.add(pos)
        for dir in OFFSETS: 
            new_pos = (pos[0] + dir[0], pos[1] + dir[1])
            if is_within(grid, new_pos) and (new_pos not in main_loop):
                to_visit.add(new_pos)
    return visited

lines = open(0).read().split()
grid = np.array([list(l) for l in lines])

startI, startJ = np.where(grid == 'S')
start_pos = startI[0], startJ[0]
start_dir = find_start_dir(grid, start_pos)

loop = set()
inner_left, inner_right = set(), set()
current_pos, current_dir = start_pos, start_dir
while True:
    loop.add(current_pos)
    old_dir = current_dir
    match move(grid, current_pos, current_dir):
        case None:
            raise Exception("What happened")
        case "loop":
            break
        case current_pos, current_dir:
            for side_dir in [old_dir, current_dir]:
                right = side(grid, current_pos, side_dir, -1)
                if right:
                    right_pos, right_pipe = right
                    inner_right.add(right_pos)
                left = side(grid, current_pos, side_dir, 1)
                if left:
                    left_pos, left_pipe = left
                    inner_left.add(left_pos)

print(ceil(len(loop)/2))

left_fill = floodfill(grid, inner_left - loop, loop)
right_fill = floodfill(grid, inner_right - loop, loop)

print(len(left_fill))
print(len(right_fill))
