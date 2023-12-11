def lookup(map: list[list[int]], seeds: list[tuple[int, int]]) -> list[tuple[int, int]]:
    result = []
    for dst, src, width in map:
        remaining = []
        for ss, se in seeds:
            os, oe = max(ss, src), min(se, src+width)
            if se <= src or ss >= src + width: # seeds fully outside range
                remaining.append((ss, se))
            elif os == ss and oe == se: # seeds fully within range
                result.append((os - src + dst, oe - src + dst))
            elif oe == se: # seeds overlap before range
                remaining.append((ss, src))
                result.append((os - src + dst, oe - src + dst))
            elif os == ss: # seeds overlap after range
                remaining.append((src + width, se))
                result.append((os - src + dst, oe - src + dst))
        seeds = remaining
    return result + seeds

def solve(seeds):
    result = float('inf')
    for seed in seeds:
        out = [seed]
        for map in maps: 
            out = lookup(map, out)
        for s, _ in out:
            result = min(s, result)
    return result

inputs, *rest = open(0).read().strip().split("\n\n")
inputs = [int(x) for x in inputs.split()[1:]]
maps = [
    [[int(x) for x in line.split()] for line in r.splitlines()[1:]]
    for r in rest
]

seeds_part1 = [(s, s + 1) for s in inputs]

seeds_part2 = []
for i in range(0, len(inputs), 2):
    seeds_part2.append((inputs[i], inputs[i] + inputs[i+1]))

print(solve(seeds_part1))
print(solve(seeds_part2))
