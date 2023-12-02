from math import prod
from sys import stdin

def game_is_correct(game):
    return all(
        segment['red'] <= 12 and segment['green'] <= 13 and segment['blue'] <= 14
        for segment in game
    )

def power(game):
    max_values = { 'red': 0, 'green': 0, 'blue': 0 }
    for segment in game:
        for col in segment:
            max_values[col] = max(max_values[col], segment[col])
    return prod(max_values[col] for col in max_values)

games = {}
for i, line in enumerate(stdin, start=1):
    _, segment_text = line.split(":")
    parsed_segments = []
    for s in segment_text.split(";"):
        rgb = { 'red': 0, 'green': 0, 'blue': 0 }
        color_pairs = s.split(",")
        for cp in color_pairs:
            n, col = cp.split()
            rgb[col] = int(n)
        parsed_segments.append(rgb)
    games[i] = parsed_segments

part1 = sum(i for i in games if game_is_correct(games[i]))
print(part1)

part2 = sum(power(games[i]) for i in games)
print(part2)
