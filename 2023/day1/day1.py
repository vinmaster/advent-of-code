from pathlib import Path
import re

def part1(input):
  lines = input.split('\n')
  sum = 0 
  for line in lines:
    matches = re.findall(r"\d", line)
    if len(matches) == 0:
      continue
    sum += int(str(matches[0] + matches[-1]))
  return sum

def part2(input):
  lines = input.split('\n')
  numbers = ['','one','two','three','four','five','six','seven','eight','nine'] 
  sum = 0 
  for line in lines:
    matchesInt = [[m.span()[0], m.group()] for m in re.finditer(r'\d', line)]
    matchWords = [[m.span()[0], str(numbers.index(m.group(1)))] for m in re.finditer(r'(?=(one|two|three|four|five|six|seven|eight|nine))', line)]
    matches = matchesInt + matchWords
    if len(matches) == 0:
      continue
    matches.sort(key=lambda pair: pair[0])
    # matches = list(map(lambda m: m[1], matches))
    matches = [m[1] for m in matches] 
    sum += int(str(matches[0] + matches[-1]))
  return sum

if __name__ == '__main__':
  inputPath = Path(__file__).parent.resolve() / 'input.txt'
  input = Path(inputPath).read_text().strip()

#   input = """
# two1nine
# eightwothree
# abcone2threexyz
# xtwone3four
# 4nineeightseven2
# zoneight234
# 7pqrstsixteen
#   """.strip()

  print(f'Part 1: {part1(input)}')
  print(f'Part 2: {part2(input)}')
