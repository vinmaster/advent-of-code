use std::{fs};

pub fn part1() {
    let file = fs::read_to_string("2018/day1/input.txt").expect("FileFail");

    let freq = file
        .split_whitespace()
        .fold(0, |acc, it| acc + it.parse::<i64>().expect("ParseFail"));

    println!("day1 part1: {}", freq);
}
