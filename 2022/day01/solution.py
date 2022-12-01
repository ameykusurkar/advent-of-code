import sys

def parse(string: str) -> list[list[int]]:
    blocks = string.split("\n\n")
    return [[int(s) for s in b.split("\n")] for b in blocks]

path = sys.argv[1]
print(f"File: {path}")

f = open(path, "r")
parsed = parse(f.read())

sums = list(sum(p) for p in parsed)
sums.sort(reverse=True)

# Part 1
print(sum(sums[:1]))

# Part 2
print(sum(sums[:3]))
