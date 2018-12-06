#!/usr/bin/env ruby
require 'open3'

def main
  year = ARGV[0]
  day = ARGV[1]

  if ARGV.length < 2
    day = year
    year = Time.new.year
  end

  puts "Running year #{year} and day #{day}"
  stdout, stderr, status = Open3.capture3("ruby ./#{year}/day#{day}/day#{day}.rb")
  if !stderr.empty?
    raise stderr
  end
  puts stdout
end

if __FILE__ == $0
  main
end
