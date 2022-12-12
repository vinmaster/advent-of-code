from pathlib import Path
from os import path

def part1(input):
  lines = input.split('\n\n')

  everyone = []
  for line in lines:
    calories = sum(list(map(int, line.split('\n'))))
    everyone.append(calories)

  return max(everyone)

def part2(input):
  lines = input.split('\n\n')

  everyone = []
  for line in lines:
    calories = sum(list(map(int, line.split('\n'))))
    everyone.append(calories)

  everyone.sort(reverse=True)
  return sum(everyone[:3])

if __name__ == '__main__':
  inputPath = Path(__file__).parent.resolve() / 'input.txt'
  input = Path(inputPath).read_text().strip()

#   input = """
# 1000
# 2000
# 3000

# 4000

# 5000
# 6000

# 7000
# 8000
# 9000

# 10000
#   """.strip()

  print(f'Part 1: {part1(input)}')
  print(f'Part 2: {part2(input)}')
