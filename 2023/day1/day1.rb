
file = File.open(File.join(File.dirname(__FILE__), 'input.txt'), 'r')
input = file.read

# input = <<~EOS
# two1nine
# eightwothree
# abcone2threexyz
# xtwone3four
# 4nineeightseven2
# zoneight234
# 7pqrstsixteen
# EOS

def part1(input)
  lines = input.strip.split("\n")
  sum = 0
  lines.each do |line|
    matches = line.to_enum(:scan, /\d/).map { Regexp.last_match }
    next if matches.length() == 0 
    matches = matches.map { |m| m.to_a[0] }
    sum += (matches.at(0) + matches.at(-1)).to_i
  end
  sum
end

def part2(input)
  lines = input.strip.split("\n")
  numbers = ['','one','two','three','four','five','six','seven','eight','nine'] 
  sum = 0
  lines.each do |line|
    matchInts = line.to_enum(:scan, /\d/).map { Regexp.last_match }
    matchInts = matchInts.map { |m| [m.offset(0)[0], m.to_a[0]] }
    matchWords = line.to_enum(:scan, /(?=(one|two|three|four|five|six|seven|eight|nine))/).map { Regexp.last_match }
    matchWords = matchWords.map { |m| [m.offset(0)[0], numbers.index(m.to_a[1]).to_s] }
    matches = matchInts + matchWords
    next if matches.length() == 0 
    matches = matches.sort_by { |m| m[0] }
    matches = matches.map { |m| m[1] }
    sum += (matches.at(0) + matches.at(-1)).to_i
  end
  sum
end

puts "part1: #{part1(input)}"
puts "part2: #{part2(input)}"
