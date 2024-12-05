defmodule Main do
  def part1(input) do
    lines = input |> String.trim |> String.split("\n")
    
    list1 = lines |> Enum.map(fn line -> String.split(line, "   ") |> List.first |> Integer.parse |> elem(0) end) |> Enum.sort
    list2 = lines |> Enum.map(fn line -> String.split(line, "   ") |> List.last |> Integer.parse |> elem(0) end) |> Enum.sort

    #IO.inspect Enum.zip(list1, list2) |> Enum.map(fn tuple -> Tuple.to_list(tuple) end)
    Enum.zip(list1, list2) |> Enum.map(fn tuple -> apply(Kernel, :-, Tuple.to_list(tuple)) end) |> Enum.map(&abs/1) |> Enum.sum
  end
  
  def part2(input) do
    lines = input |> String.trim |> String.split("\n")
    
    list1 = lines |> Enum.map(fn line -> String.split(line, "   ") |> List.first |> Integer.parse |> elem(0) end)
    list2 = lines |> Enum.map(fn line -> String.split(line, "   ") |> List.last |> Integer.parse |> elem(0) end)

    Enum.map(list1, fn x -> x * Enum.count(list2, &(&1 == x)) end) |> Enum.sum
  end
end

input = """
3   4
4   3
2   5
1   3
3   9
3   3
"""

IO.puts "part1: #{Main.part1(input)}"
IO.puts "part2: #{Main.part2(input)}"