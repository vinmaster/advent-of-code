#!/usr/bin/env rust-script
//! Dependencies can be specified in the script file itself as follows:
//!
//! ```cargo
//! [dependencies]
//! regex = "1.10.2"
//! ```

extern crate regex;
use regex::Regex;
use std::fs;

fn part1(input: &str) {
    let lines = input.split("\n").collect::<Vec<&str>>();
    // println!("{:?}", lines);
    let re = Regex::new(r"\d").unwrap();

    let mut sum = 0;
    for line in lines.iter() {
        let matches: Vec<_> = re.find_iter(line).map(|m| m.as_str()).collect();
        let first = matches.first().unwrap();
        let last = matches.last().unwrap();
        // match first {
        //     None => _
        // }
        let num = (first.to_owned().to_owned() + last.to_owned()).parse::<i32>().unwrap();
        sum += num
    }

    println!("part1: {:?}", sum);
}

fn part2(input: &str) {
    let _lines = input.split("\n").collect::<Vec<&str>>();
    // for capture in re.captures_iter(line).map(|c| c.extract()) {
            // let index = capture.iter().enumerate()
            //     .skip(1)                  // skip the first group
            //     .find(|t| t.1.is_some())  // find the first `Some`
            //     .map(|t| t.0)             // extract the index
            //     .unwrap_or(0);            // get the index
            // let index: (&str, [&str]) = capture.extract();
        // }
    // println!("part1: {:?}", sum);
    println!("part1: TODO");
}

fn main() {
    let path = "2023/day1/input.txt";
    let input = fs::read_to_string(path)
        .expect("Should have been able to read the file")
        .trim()
        .to_string();

    // let input = "
    // 1abc2
    // pqr3stu8vwx
    // a1b2c3d4e5f
    // treb7uchet"
    //     .trim()
    //     .to_string();

    // let input = "
    // two1nine
    // eightwothree
    // abcone2threexyz
    // xtwone3four
    // 4nineeightseven2
    // zoneight234
    // 7pqrstsixteen"
    //     .trim()
    //     .to_string();

    part1(&input);
    part2(&input);
}
