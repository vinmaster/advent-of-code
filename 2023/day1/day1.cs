#!/usr/bin/env dotnet-script
using System.Text.RegularExpressions;

string input = GetInput();

// input = @"
// 1abc2
// pqr3stu8vwx
// a1b2c3d4e5f
// treb7uchet";

// input = @"
// two1nine
// eightwothree
// abcone2threexyz
// xtwone3four
// 4nineeightseven2
// zoneight234
// 7pqrstsixteen
// ";

Console.WriteLine("part1: " + Day1(input));
Console.WriteLine("part2: " + Day2(input));

public static int Day1(string input)
{
  string[] lines = input.Trim().Split("\n");
  var sum = 0;
  foreach (var line in lines)
  {
    var matches = Regex.Matches(line, @"\d").OfType<Match>().Select(m => (m.Index, int.Parse(m.Value))).ToList();
    var (firstIndex, first) = matches.FirstOrDefault();
    var (lastIndex, last) = matches.LastOrDefault();
    var num = $"{first}{last}";
    sum += int.Parse(string.IsNullOrEmpty(num) ? "0" : num);
  }
  return sum;
}

public static int Day2(string input)
{
  string[] lines = input.Trim().Split("\n");
  var sum = 0;
  var numbers = new List<string> { string.Empty, "one", "two", "three", "four", "five", "six", "seven", "eight", "nine" };
  foreach (var line in lines)
  {
    var matches = Regex.Matches(line, @"\d").OfType<Match>().Select(m => (m.Index, int.Parse(m.Value))).ToList();
    var matchesWords = numbers.SelectMany((n, i) => line.AllIndexesOf(n).Select(index => (index, i))).ToList();
    matches.AddRange(matchesWords);
    matches = matches.OrderBy(x => x.Item1).ToList();
    var (firstIndex, first) = matches.FirstOrDefault();
    var (lastIndex, last) = matches.LastOrDefault();
    var num = $"{first}{last}";
    sum += int.Parse(string.IsNullOrEmpty(num) ? "0" : num);
  }
  return sum;
}

public static List<int> AllIndexesOf(this string str, string target)
{
  if (target == null) throw new ArgumentException("Target cannot be null", "target");
  if (target.Length == 0) return new();
  var indexes = new List<int>();
  for (int index = 0; ; index += target.Length)
  {
    index = str.IndexOf(target, index);
    if (index == -1) return indexes;
    indexes.Add(index);
  }
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
