defmodule Day1 do
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

IO.puts "part1: #{Day1.part1(input)}"
IO.puts "part2: #{Day1.part2(input)}"


"""
defmodule Day1 do
  def part1(list1, list2) do
    Enum.zip(Enum.sort(list1), Enum.sort(list2))
      |> Enum.map(& elem(&1,0)-elem(&1,1))
      |> Enum.map(&abs/1)
      |> Enum.sum
  end

  def part2(list1, list2) do
    list1
      |> Enum.map(fn x ->
        x * Enum.count(list2, & &1==x)
      end)
      |> Enum.sum
  end

  def process(input) do
    {list1, list2} = input
      |> String.split("\n")
      |> Enum.filter(fn s -> String.length(s) > 0 end)
      |> Enum.map(fn s ->
        splits = String.split(s)
        #List.to_tuple(splits)
        splits
      end)
      |> Enum.reduce({[], []}, fn [a, b],{list1, list2} ->
        {[String.to_integer(a) | list1], [String.to_integer(b) | list2]}
      end)

    {part1(list1, list2), part2(list1, list2)}  
  end
end

{result1, result2} = Day1.process(File.read!("input.txt"))
IO.puts "Result Part 1 = #{result1}"
IO.puts "Result Part 2 = #{result2}"
"""