instructions = []
for line in open(0).read().splitlines():
    dir, c, _ = line.split()
    instructions.append((dir, int(c)))
print(instructions)

curr = [0, 0]
border = set()
border.add(tuple(curr))
for dir, c in instructions:
    match dir:
        case 'U':
            for _ in range(c):
                curr[0] -= 1
                border.add(tuple(curr))
        case 'L':
            for _ in range(c):
                curr[1] -= 1
                border.add(tuple(curr))
        case 'D':
            for _ in range(c):
                curr[0] += 1
                border.add(tuple(curr))
        case 'R':
            for _ in range(c):
                curr[1] += 1
                border.add(tuple(curr))
        case _:
            raise Exception(f"{dir}: {c}")

print(border)
minx = min(x for _, x in border)
miny = min(y for y, _ in border)
maxx = max(x for _, x in border)
maxy = max(y for y, _ in border)
print((miny, minx), (maxy, maxx))

MOVES = [
  ( 1,  0), # down
  ( 0,  1), # right
  (-1,  0), # up
  ( 0, -1), # left
]

q = set()
q.add((1, 1))
visited = set()

while q:
    curr = q.pop()
    if curr in visited:
        continue
    visited.add(curr)
    for dir in MOVES:
        cand = (curr[0] + dir[0], curr[1] + dir[1])
        if cand not in border:
            q.add(cand)
print(len(visited))
print(len(border))
print(len(visited) + len(border))
