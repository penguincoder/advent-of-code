use std::fs::File;
use std::io::{BufRead, BufReader};
use std::iter::Iterator;

fn print_final_frequency(ints: &Vec<i32>) {
    let mut frequency = 0;
    for drift in ints {
        frequency = frequency + drift;
    }
    println!("frequency is {}", frequency)
}

fn print_first_duped_frequency(line_count: usize, offsets: &Vec<i32>) {
    let mut freaks: Vec<i32> = vec![0];
    let first_duped_frequency = duped_frequency(0, line_count, offsets, &mut freaks);
    println!("first duped frequency is {}", first_duped_frequency);
}

fn duped_frequency(
    index: usize,
    max_count: usize,
    offsets: &Vec<i32>,
    freaks: &mut Vec<i32>,
) -> i32 {
    let cur_offset = offsets.get(index).unwrap();
    let next_freq = freaks.last().unwrap() + cur_offset;
    let mut found = false;
    for freq in freaks.iter() {
        if freq == &next_freq {
            found = true;
            break;
        }
    }
    if found {
        next_freq
    } else {
        let next_index = (index + 1) % max_count;
        freaks.push(next_freq);
        duped_frequency(next_index, max_count, offsets, freaks)
    }
}

fn main() {
    let ints: Vec<i32> = BufReader::new(File::open("input").unwrap())
        .lines()
        .map(|line| line.unwrap().parse::<i32>().unwrap())
        .collect();
    let line_count: usize = ints.len();
    print_final_frequency(&ints);
    print_first_duped_frequency(line_count, &ints)
}
