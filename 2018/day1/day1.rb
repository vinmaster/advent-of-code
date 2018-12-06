
file = File.open(File.join(File.dirname(__FILE__), 'input.txt'), 'r')
lines = file.read

total = lines.strip.split("\n").map(&:to_i).sum
puts "day1 part1: #{total}"
