from collections import Counter
from sys import stdin

def parse(line: str) -> tuple[list[int], int]:
    cards, bid = line.split()
    return [parse_card(c) for c in cards], int(bid)

def parse_card(c: str) -> int:
    match c:
        case 'T':
            return 10
        case 'J':
            return 11
        case 'Q':
            return 12
        case 'K':
            return 13
        case 'A':
            return 14
        case i:
            return int(i)

def hand_type(hand: list[int]) -> int:
    jokers = hand.count(1)
    match sorted(Counter(hand).values()):
        case [5]:
            return 7
        case [1, 4]:
            return 7 if jokers else 6
        case [2, 3]:
            return 7 if jokers else 5 
        case [1, 1, 3]:
            return 6 if jokers else 4
        case [1, 2, 2]:
            return 4 + jokers if jokers else 3
        case [1, 1, 1, 2]:
            return 4 if jokers else 2
        case [1, 1, 1, 1, 1]:
            return 2 if jokers else 1
        case _:
            raise Exception(f"cannot score {hand}")

def jokerize(parsed: tuple[list[int], int]) -> tuple[list[int], int]:
    cards, bid = parsed
    return [1 if c == 11 else c for c in cards], bid

def sort_key(item) -> str:
    hand, _ = item
    ht = hand_type(hand)
    return "".join(f"{i:X}" for i in [ht] + hand)

def score(parsed) -> int:
    return sum(
        bid * rank
        for rank, (_, bid)
        in enumerate(sorted(parsed, key=sort_key), start=1)
    )

raw = [l for l in stdin]

parsed = [parse(l) for l in raw]
part1 = score(parsed)
print(part1)

jokerized = [jokerize(l) for l in parsed]
part2 = score(jokerized)
print(part2)
