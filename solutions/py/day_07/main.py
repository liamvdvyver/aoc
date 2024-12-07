import sys
import math


def parse_input() -> list[tuple[int, list[int]]]:
    ret = []
    for ln in sys.stdin:
        print(ln)
        cur: list[str] = ln.split(":")
        ret.append(
            (int(cur[0]), [int(num) for num in cur[1][1:].split(" ")])
        )
    return ret


def concat_op(i: int, j: int) -> int:
    return 10 ** (int(math.log10(j)) + 1) * i + j


def solve_line(
    target: int, nums: list[int], i: int, acc: int, has_concat: bool
) -> bool:
    if target == acc and i == len(nums):
        return True
    elif i == len(nums) or acc > target:
        return False
    else:
        next_num: int = nums[i]
        i += 1
        return (
            solve_line(target, nums, i, acc * next_num, has_concat)
            or solve_line(target, nums, i, acc + next_num, has_concat)
            or (
                has_concat
                and solve_line(
                    target,
                    nums,
                    i,
                    concat_op(acc, next_num),
                    has_concat,
                )
            )
        )


def add_solvable(
    input: list[tuple[int, list[int]]], has_concat: bool
) -> int:
    return sum(
        [
            target
            for target, nums in input
            if solve_line(target, nums, 1, nums[0], has_concat)
        ]
    )


if __name__ == "__main__":
    input = parse_input()
    print(add_solvable(input, False))
    print(add_solvable(input, True))
