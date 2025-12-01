# cached = {}

def solve_one(x):
    # if x in cached.keys():
    #     return cached[x]

    result = None
    if x == 0:
        result = [1]
    else:
        s = str(x)
        ln = len(s)
        if ln % 2 == 0:
            result = [int(a) for a in split(s)]
        else:
            result = [2024 * x]

    # cached[x] = result
    return result

def split(arr):
    half = len(arr) // 2
    return [arr[:half], arr[half:]]

input = [5688, 62084, 2, 3248809, 179, 79, 0, 172169]
# input = [125,17]

def iterate(input):
    result = []
    for i in input:
        result += solve_one(i)
    return result

result = input
for i in range(75):
    result = iterate(result)
    print(i)

print(len(result))
