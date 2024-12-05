import sequtils
import std/strutils
import std/algorithm

proc part1(input: string): int =
  let lines = strip(input).split("\n")
  var list1: seq[int]
  var list2: seq[int]
  var sum = 0
  for line in lines:
    let pair = line.split("   ")
    list1.add(parseInt(pair[0]))
    list2.add(parseInt(pair[1]))
  sort(list1, system.cmp)
  sort(list2, system.cmp)
  for i in 0..<list1.len:
    let distance = system.abs(list1[i] - list2[i])
    sum += distance
  sum

proc part2(input: string): int =
  let lines = strip(input).split("\n")
  var list1: seq[int]
  var list2: seq[int]
  var sum = 0
  for line in lines:
    let pair = line.split("   ")
    list1.add(parseInt(pair[0]))
    list2.add(parseInt(pair[1]))
  for i in 0..<list1.len:
    let score = list1[i] * count(list2, list1[i])
    sum += score
  sum

let input = """
3   4
4   3
2   5
1   3
3   9
3   3
"""

echo "part1: ", part1(input)
echo "part2: ", part2(input)

#[

import sugar
import sequtils
import strutils
import system
import std/algorithm

var (leftList, rightList) = stdin
    .lines
    .toSeq
    .map( line => line
                  .split(" ", 2)
                  .map(part => part.strip())
                  .filter( part => part != "")
                  .map(parseInt)
    )
    .map( line => (line[0], line[1]) )
    .unzip
leftList.sort()
rightList.sort()
echo(
    leftList
    .zip(rightList)
    .toSeq
    .map( row => ( abs(row[0] - row[1]) ) )
    .foldl( a+b )
)

var (leftList, rightList) = stdin
    .lines
    .toSeq
    .map( line => line
                  .split(" ", 2)
                  .map( part => part.strip() )
                  .filter( part => part != "" )
                  .map(parseInt)
    )
    .map( line => (line[0], line[1]) )
    .unzip
echo(
    leftList
    .map( left => (
        left * rightList.filter( right => left == right ).len()
    ))
    .foldl( a + b )
)
]#