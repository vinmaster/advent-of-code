file = File.open(File.join(File.dirname(__FILE__), 'input.txt'), 'r')
input = file.read

input = <<~EOS
3   4
4   3
2   5
1   3
3   9
3   3
EOS

def part1(input)
  lines = input.strip.split("\n")
  list1 = []
  list2 = []
  lines.each do |line|
    num1, num2 = line.split("   ").map { |s| s.to_i }
    list1.append(num1)
    list2.append(num2)
  end
  list1.sort!
  list2.sort!
  list1.zip(list2).map { |pair| (pair[0] - pair[1]).abs }.sum
end

def part2(input)
  lines = input.strip.split("\n")
  list1 = []
  list2 = []
  lines.each do |line|
    num1, num2 = line.split("   ").map { |s| s.to_i }
    list1.append(num1)
    list2.append(num2)
  end
  list1.map { |num| num * list2.count(num) }.sum
end

puts "part1: #{part1(input)}"
puts "part2: #{part2(input)}"

=begin

input = File
  .readlines('input01.txt')
  .map { |line| line.scan(/\d+/).map &:to_i }


# Part 1
p input.transpose.map(&:sort).transpose.sum { |a,b| (a - b).abs }


# Part 2
a,b = input.transpose
p a.sum { |i| i * b.count(i) }

=end
