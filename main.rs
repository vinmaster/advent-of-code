use std::{env};
extern crate year2018day1;
// use year2018day1::part1;

fn main() {
    let args: Vec<String> = env::args().collect();
    let year = &args[1];
    let day = &args[2];

    // println!("length {}", args.len());
    if args.len() < 3 {
        // day = year;
        // year = &String::from("2018"); // TODO get current year
    }
    println!("Running year {} and day {}", year, day);
    year2018day1::part1();

    // let output = include!("./2018/day1/day1.rs");
    // let path = format!("./{}/day{}/day{}.rs", year, day, day);
    // println!("{}", path);
    // let output = include!(path);
}
