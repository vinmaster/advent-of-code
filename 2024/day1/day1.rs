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
