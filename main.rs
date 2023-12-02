#!/usr/bin/env rust-script
//! Dependencies can be specified in the script file itself as follows:
//!
//! ```cargo
//! [dependencies]
//! rand = "0.8.0"
//! time = "0.3.17"
//! ```

// use rand::prelude::*;
use std::env;
use std::process::Command;
use std::io::{self, Write};

fn main() {
    let args: Vec<String> = env::args().collect();
    // dbg!(args);
    // let x: u64 = random();
    // println!("A random number: {}", x);

    let year: i32;
    let day: i32;
    if args.len() == 2 {
        year = time::OffsetDateTime::now_utc().date().year();
        day = args[1].parse::<i32>().unwrap();
    } else {
        year = args[1].parse::<i32>().unwrap();
        day = args[2].parse::<i32>().unwrap();
    }
    // println!("Running: {} {}", year, day);
    
    let path = format!("{year}/day{day}/day{day}.rs");
    let output = Command::new("rust-script")
        .args([&path])
        .output()
        .expect("failed");
    io::stdout().write_all(&output.stdout).unwrap();
    io::stdout().write_all(&output.stderr).unwrap();
}
