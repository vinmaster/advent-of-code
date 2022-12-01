#!/usr/bin/env rust-script

use std::fs;

fn part1(input: &str) {
    let lines = input.split("\n\n").collect::<Vec<_>>();
    // println!("{:?}", lines);

    let mut calories = Vec::new();
    for line in lines.iter() {
        let c: i32 = line
            .split("\n")
            .map(|s| s.trim().parse::<i32>().unwrap())
            .sum();
        calories.push(c);
    }

    println!("part1: {:?}", calories.iter().max().unwrap());
}

fn part2(input: &str) {
    let lines = input.split("\n\n").collect::<Vec<_>>();

    let mut calories = Vec::new();
    for line in lines.iter() {
        let c: i32 = line
            .split("\n")
            .map(|s| s.trim().parse::<i32>().unwrap())
            .sum();
        calories.push(c);
    }
    calories.sort();
    calories.reverse();

    println!("part2: {:?}", &calories[0..3].iter().sum::<i32>());
}

fn main() {
    let path = "2022/day1/input.txt";
    let input = fs::read_to_string(path)
        .expect("Should have been able to read the file")
        .trim()
        .to_string();

    //     let input = "
    // 1000
    // 2000
    // 3000

    // 4000

    // 5000
    // 6000

    // 7000
    // 8000
    // 9000

    // 10000"
    //         .trim()
    //         .to_string();

    part1(&input);
    part2(&input);
}
