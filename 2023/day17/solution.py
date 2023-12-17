import numpy as np
import heapq

MOVES = [
  ( 1,  0), # down
  ( 0,  1), # right
  (-1,  0), # up
  ( 0, -1), # left
]

def flip(point: tuple[int, int]) -> tuple[int, int]:
    return (point[0] * -1, point[1] * -1)

def is_within(grid: np.ndarray, pos: tuple[int, int]) -> bool:
    target = grid.shape
    return 0 <= pos[0] < target[0] and 0 <= pos[1] < target[1]

def vertices(grid, max_step):
    rows, cols = grid.shape
    for i in range(rows):
        for j in range(cols):
            for dir in MOVES:
                for steps in range(max_step + 1):
                    yield((i, j), dir, steps)

def neighbors(grid, min_step, max_step, v):
    pos, dir, steps = v
    for new_dir in MOVES:
        if new_dir == flip(dir):
            continue
        if new_dir == dir and steps >= max_step:
                continue
        if new_dir != dir and 1 <= steps < min_step:
            continue
        new_pos = (pos[0] + new_dir[0], pos[1] + new_dir[1])
        if not is_within(grid, new_pos):
            continue
        new_steps = 1
        if new_dir == dir:
            new_steps += steps
        yield((new_pos, new_dir, new_steps), grid[new_pos])

def min_cost(grid: np.ndarray, min_step, max_step):
    heap, dist = [], {}
    for v in vertices(grid, max_step):
        dist[v] = float('inf')
        heap.append((float('inf'), v))
    heapq.heapify(heap)
    dist[((0, 0), MOVES[0], 0)] = 0
    rows, cols = grid.shape
    dest = set()
    for dir in MOVES:
        for s in range(min_step, max_step + 1):
            dest.add(((rows - 1, cols - 1), dir, s))
    explored = set()
    while not (dest & explored): 
        _, v = heapq.heappop(heap)
        while v in explored:
            _, v = heapq.heappop(heap)
        explored.add(v)
        for w, cost in neighbors(grid, min_step, max_step, v):
            new_dist = dist[v] + cost
            if new_dist < dist[w]:
                heapq.heappush(heap, (new_dist, w))
                dist[w] = new_dist
    k = (dest & explored).pop()
    return dist[k]

lines = open(0).read().split()
grid = np.array([[int(i) for i in l] for l in lines])

print(min_cost(grid, min_step=1, max_step=3))
print(min_cost(grid, min_step=4, max_step=10))
