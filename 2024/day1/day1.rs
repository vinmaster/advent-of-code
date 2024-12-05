#!/usr/bin/env rust-script

use std::fs;

fn part1(input: &str) {
    let lines = input.split("\n").collect::<Vec<&str>>();
    // println!("{:?}", lines);
    let mut list1 = vec![];
    let mut list2 = vec![];
    let mut sum = 0;

    for line in lines.iter() {
        let nums: Vec<i32> = line.split("   ").map(|s| s.parse::<i32>().unwrap()).collect();
        list1.push(nums.clone().into_iter().nth(0).unwrap());
        list2.push(nums.clone().into_iter().nth(1).unwrap());
    }
    list1.sort();
    list2.sort();
    for i in 0..(list1.len()) {
        let distance = (list1.clone().into_iter().nth(i).unwrap() - list2.clone().into_iter().nth(i).unwrap()).abs();
        sum += distance;
    }

    println!("part1: {:?}", sum);
}

fn part2(input: &str) {
    let lines = input.split("\n").collect::<Vec<&str>>();
    let mut list1 = vec![];
    let mut list2 = vec![];
    let mut sum = 0;

    for line in lines.iter() {
        let nums: Vec<i32> = line.split("   ").map(|s| s.parse::<i32>().unwrap()).collect();
        list1.push(nums.clone().into_iter().nth(0).unwrap());
        list2.push(nums.clone().into_iter().nth(1).unwrap());
    }
    for i in 0..(list1.len()) {
        let left = list1.clone().into_iter().nth(i).unwrap();
        let right = list2.clone().iter().filter(|&n| *n == left).count() as i32;
        let score = left * right;
        sum += score;
    }

    println!("part1: {:?}", sum);
}

fn main() {
    let path = "2024/day1/input.txt";
    let input = fs::read_to_string(path)
        .expect("Should have been able to read the file")
        .trim()
        .to_string();

//     let input = "
// 3   4
// 4   3
// 2   5
// 1   3
// 3   9
// 3   3
// ".trim().to_string();

    part1(&input);
    part2(&input);
}


/*

fn part1(s: &str) -> i32 {
  let (left, right) = parse(s);
  left.sorted()
    .zip(right.sorted())
    .map(|(a, b)| a.abs_diff(b) as i32)
    .sum()
}

fn part2(s: &str) -> i32 {
  let (left, right) = parse(s);
  let counts = right.fold(HashMap::<_, i32>::new(), |mut m, x| {
    *m.entry(x).or_default() += 1;
    m
  });
  left.map(|x| x * counts.get(&x).copied().unwrap_or_default()).sum()
}

fn parse(s: &str) -> (impl Iterator<Item=i32>, impl Iterator<Item=i32>) {
  let nums = s.split_whitespace().map(i32::from_str).map(Result::unwrap);
  let left = nums.clone().step_by(2);
  let right = nums.clone().skip(1).step_by(2);
  (left, right)
}

--------------------------------------------------------------------------------

use std::{collections::HashMap, io::Read};

fn parse_input(input: &str) -> (Vec<i32>, Vec<i32>) {
    input.lines().map(|line| {
        let mut words = line.split_whitespace().map(|word| word.parse::<i32>().unwrap());
        (words.next().unwrap(), words.next().unwrap())
    }).unzip()
}

fn part_1(input: &str) -> i32 {
    let (mut a, mut b) = parse_input(input);

    a.sort_unstable();
    b.sort_unstable();

    a.into_iter().zip(b).map(|(a, b)| (a - b).abs()).sum()
}

fn part_2(input: &str) -> i32 {
    let (a, b) = parse_input(input);

    let mut b_counts = HashMap::new();
    for i in b {
        *b_counts.entry(i).or_insert(0) += 1;
    }

    a.into_iter().map(|x| x * b_counts.get(&x).copied().unwrap_or(0)).sum()
}

fn main() {
    let mut input = String::new();
    std::io::stdin().read_to_string(&mut input).unwrap();

    let start_time = std::time::Instant::now();
    let result = part_1(&input);
    println!("Part 1 time: {:?}", std::time::Instant::now() - start_time);
    println!("Part 1 result: {}", result);

    let start_time = std::time::Instant::now();
    let result = part_2(&input);
    println!("Part 2 time: {:?}", std::time::Instant::now() - start_time);
    println!("Part 2 result: {}", result);
}

*/