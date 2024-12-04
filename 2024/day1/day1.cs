#!/usr/bin/env dotnet-script

string input = GetInput();

// input = @"
// 3   4
// 4   3
// 2   5
// 1   3
// 3   9
// 3   3";

Console.WriteLine("part1: " + Part1(input));
Console.WriteLine("part2: " + Part2(input));

public static int Part1(string input)
{
  string[] lines = input.Trim().Split("\n");
  var sum = 0;
  var list1 = new List<int>();
  var list2 = new List<int>();
  foreach (var line in lines)
  {
    var nums = line.Split("   ").Select(Int32.Parse).ToList();
    list1.Add(nums[0]);
    list2.Add(nums[1]);
  }
  list1.Sort();
  list2.Sort();
  for (int i = 0; i < list1.Count; i++)
  {
    var distance = Math.Abs(list1[i] - list2[i]);
    sum += distance;
  }
  return sum;
}

public static int Part2(string input)
{
  string[] lines = input.Trim().Split("\n");
  var sum = 0;
  var list1 = new List<int>();
  var list2 = new List<int>();
  foreach (var line in lines)
  {
    var nums = line.Split("   ").Select(Int32.Parse).ToList();
    list1.Add(nums[0]);
    list2.Add(nums[1]);
  }
  for (int i = 0; i < list1.Count; i++)
  {
    var score = list1[i] * list2.Where(x => x == list1[i]).Count();
    sum += score;
  }
  return sum;
}

public static string GetInput()
{
  string command = System.Environment.CommandLine;
  string filename = command.Split(' ')[1];
  string puzzlePath = Path.Join(System.Environment.CurrentDirectory, filename);
  string inputPath = Path.Join(System.IO.Path.GetDirectoryName(puzzlePath), "input.txt");
  string input = File.ReadAllText(inputPath);
  return input;
}
