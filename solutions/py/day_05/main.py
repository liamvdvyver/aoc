import sys


def parse_input() -> tuple[set[tuple[int, int]], list[list[int]]]:

    rules: set[tuple[int, int]] = set()
    updates: list[list[int]] = []

    parsing_updates: bool = False
    for line in sys.stdin:

        if line == "\n":
            parsing_updates = True
            continue

        elif parsing_updates:
            updates.append([int(num) for num in line.split(",")])

        else:

            part = line.split("|")
            rules.add((int(part[0]), int(part[1])))

    return rules, updates


def is_sorted(nums: list[int], rules: set[tuple[int, int]]) -> bool:
    n = len(nums)
    rev_ordered_pairs: set[tuple[int, int]] = set()
    for i in range(0, n):
        for j in range(i + 1, n):
            rev_ordered_pairs.add((nums[j], nums[i]))

    return rev_ordered_pairs.isdisjoint(rules)


def middle_num(nums: list[int]):
    return nums[int(len(nums) / 2)]


def solve_part_one(
    updates: list[list[int]], rules: set[tuple[int, int]]
):
    return sum(
        [
            middle_num(nums)
            for nums in updates
            if is_sorted(nums, rules)
        ]
    )


def make_sorted(
    nums: list[int], rules: set[tuple[int, int]], acc: list[int]
) -> list[int] | None:

    if len(nums) == 0:
        return acc

    relevant_rules: set[tuple[int, int]] = get_relevant_rules(
        nums, rules
    )

    for num in nums:

        if (
            len(
                [
                    (fst, snd)
                    for (fst, snd) in relevant_rules
                    if num == snd
                ]
            )
            == 0
        ):

            new_nums, new_acc = nums.copy(), acc.copy()
            new_nums.remove(num)
            new_acc.append(num)

            ret = make_sorted(new_nums, rules, new_acc)

            if ret is not None:
                return ret

    return None


def get_relevant_rules(nums: list[int], rules: set[tuple[int, int]]):
    return set(
        [
            (fst, snd)
            for (fst, snd) in rules
            if fst in nums and snd in nums
        ]
    )


def solve_part_two(
    updates: list[list[int]], rules: set[tuple[int, int]]
) -> int:

    return sum(
        [
            middle_num(make_sorted(nums, rules, []) or [])
            for nums in updates
            if not is_sorted(nums, rules)
        ]
    )


if __name__ == "__main__":
    rules, updates = parse_input()

    print(solve_part_one(updates, rules))
    print(solve_part_two(updates, rules))
