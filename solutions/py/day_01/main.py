import sys


def get_freq(ints: list[int]) -> dict[int, int]:
    ret: dict[int, int] = {}
    for i in ints:
        if ret.get(i):
            ret[i] += 1
        else:
            ret[i] = 1
    return ret


def part_one(l1: list[int], l2: list[int]) -> int:
    ret = 0
    l1.sort()
    l2.sort()
    while len(l1) > 0 and len(l2) > 0:
        ret += abs(l1.pop() - l2.pop())
    return ret


def part_two(l1: list[int], l2: list[int]) -> int:
    f1 = get_freq(l1)
    f2 = get_freq(l2)
    ret = 0
    for k, v in f1.items():
        ret += k * v * (f2.get(k) or 0)
    return ret


def parse() -> tuple[list[int], list[int]]:
    l1: list[int] = []
    l2: list[int] = []
    for line in sys.stdin:
        cur = line.split("   ")
        l1.append(int(cur[0]))
        l2.append(int(cur[1]))
    return l1, l2


if __name__ == "__main__":
    l1, l2 = parse()
    print(part_one(l1.copy(), l2.copy()))
    print(part_two(l1, l2))
