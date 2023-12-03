import numpy as np
import sys

lines = sys.stdin.read().split()
parsed = np.array([list(l) for l in lines])
padded = np.pad(parsed, (1, 1), constant_values='.')

D = '0123456789'

def has_symbols(area: np.ndarray) -> bool:
    not_syms = D + '.'
    return any(c for c in area.ravel() if c not in not_syms)

part1 = 0

rows, cols = padded.shape
for i in range(1, rows):
    for j in range(1, cols):
        if padded[i, j] in D and padded[i, j-1] not in D:
            end_j = next(idx for (idx, n) in enumerate(padded[i, j:]) if n not in D)
            area = padded[i-1:i+2, j-1:j+1+end_j]
            if has_symbols(area):
                arr = padded[i, j:j+end_j]
                part1 += int(''.join(arr))

print(part1)
