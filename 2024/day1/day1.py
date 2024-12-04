from pathlib import Path


def part1(input):
    lines = input.split("\n")
    sum = 0
    list1 = []
    list2 = []
    for line in lines:
        n1, n2 = line.split("   ")
        list1.append(int(n1))
        list2.append(int(n2))
    list1 = sorted(list1)
    list2 = sorted(list2)

    for pair in list(zip(list1, list2)):
        sum += abs(pair[0] - pair[1])

    return sum


def part2(input):
    lines = input.split("\n")
    list1 = []
    list2 = []
    for line in lines:
        n1, n2 = line.split("   ")
        list1.append(int(n1))
        list2.append(int(n2))

    return sum([num * list2.count(num) for num in list1])


if __name__ == "__main__":
    inputPath = Path(__file__).parent.resolve() / "input.txt"
    input = Path(inputPath).read_text().strip()

    #     input = """
    # 3   4
    # 4   3
    # 2   5
    # 1   3
    # 3   9
    # 3   3
    #   """.strip()

    print(f"Part 1: {part1(input)}")
    print(f"Part 2: {part2(input)}")
