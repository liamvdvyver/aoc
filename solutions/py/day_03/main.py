import sys

import re

p1_re = re.compile("mul\\([0-9]+,[0-9]+\\)")


def add_mults(line: str) -> int:
    ret = 0
    for m in p1_re.finditer(line):
        nums: list[str] = line[
            (m.span()[0] + 4) : (m.span()[1] - 1)
        ].split(",")
        a, b = int(nums[0]), int(nums[1])
        # print(f"{a}, {b}")
        ret += a * b
    return ret


def active_chunks(line: str) -> list[tuple[bool, str]]:
    ret = []
    active_re = re.compile("do\\(\\)|don't\\(\\)")

    prev_end = 0
    prev_substr = "do()"

    for m in active_re.finditer(line):

        ret.append(
            (prev_substr == "do()", line[prev_end : m.span()[0]])
        )

        prev_substr = line[m.span()[0] : m.span()[1]]
        prev_end = m.span()[1]

    ret.append((prev_substr == "do()", line[prev_end:]))

    print(ret)
    return ret


def solve_part_one(lines: list[str]) -> int:
    return sum(add_mults(line) for line in lines)


def solve_part_two(lines: list[str]) -> int:
    ret = 0
    joined = ""
    for line in lines:
        joined += line
    for match, part in active_chunks(joined):
        if match:
            print()
            print(part)
            ret += add_mults(part)
            print(add_mults(part))
    return ret


if __name__ == "__main__":
    input: list[str] = [line for line in sys.stdin]
    print(solve_part_one(input))
    print(solve_part_two(input))
