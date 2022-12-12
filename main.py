import datetime
import subprocess
import sys

if __name__ == '__main__':
  input = ''
  args = sys.argv
  if len(args) == 2:
    year = datetime.date.today().year
    day = args[1]
  elif len(args) == 3:
    year = args[1]
    day = args[2]

  print('Running', year, day)

  subprocess.run(['python3', f'{year}/day{day}/day{day}.py'])
