#!/usr/bin/env ruby
require 'open3'

def main
  # Provide year and day
  year = ARGV[0]
  day = ARGV[1]

  if ARGV.length == 1 && ARGV[0].length > 2
    # Only year was provided
    puts "Running year #{year}"
    days, stderr, status = Open3.capture3("ls ./#{year}")
    days = days.strip.split("\n");
    days.each do |day|
      next unless File.file?("./#{year}/#{day}/#{day}.rb")

      stdout, stderr, status = Open3.capture3("ruby ./#{year}/#{day}/#{day}.rb")
      if !stderr.empty?
        raise stderr
      end
      puts stdout
    end
  else
    if ARGV.length == 1
      # Only day was provided
      day = year
      year = Time.new.year
    end
    # puts "Running year #{year} day #{day}"
    stdout, stderr, status = Open3.capture3("ruby ./#{year}/day#{day}/day#{day}.rb")
    if !stderr.empty?
      raise stderr
    end
    puts stdout
  end
end

if __FILE__ == $0
  main
end
