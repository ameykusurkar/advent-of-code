from itertools import takewhile

SOLVED = {}

def cache(ss, ns, i):
    SOLVED[ss, ns] = i
    return i

def solve(ss: str, ns) -> int:
    if (ss, ns) in SOLVED:
        return SOLVED[ss, ns]
    if not ss:
        if not ns:
            return 1
        else:
            return 0
    match ss[0]:
        case '.':
            return cache(ss, ns, solve(ss[1:], ns))
        case '?':
            return cache(ss, ns, solve('#' + ss[1:], ns) + solve('.' + ss[1:], ns))
        case '#':
            if not ns: 
                return cache(ss, ns, 0)
            hashes = 1 + sum(1 for _ in takewhile(lambda x: x == '#', ss[1:])) 
            possib = 1 + sum(1 for _ in takewhile(lambda x: x == '?', ss[1:])) 
            n = ns[0]
            rem = ss[min(possib, n):]
            if hashes > n:
                return cache(ss, ns, 0)
            elif possib < n and not rem:
                return cache(ss, ns, 0)
            elif possib < n and rem[0] == '.':
                return cache(ss, ns, 0)
            elif possib < n:
                return cache(ss, ns, solve(rem, (n-possib,) + ns[1:]))
            elif possib >= n and not rem:
                return cache(ss, ns, solve(rem, ns[1:]))
            elif possib >= n and rem[0] == '#':
                return cache(ss, ns, 0)
            elif possib >= n:
                return cache(ss, ns, solve("." + ss[(n+1):], ns[1:]))
            else:
                raise Exception(f"Error: {ss}, {ns}")
        case _:
            raise Exception(f"Error: {ss}, {ns}")

inputs = []
for line in open(0).read().splitlines():
    l, r = line.split()
    inputs.append((l, tuple([int(x) for x in r.split(',')])))

inputs2 = [('?'.join([l for _ in range(5)]), r * 5) for l, r in inputs]

print(sum(solve(l, r) for l, r in inputs))
print(sum(solve(l, r) for l, r in inputs2))
