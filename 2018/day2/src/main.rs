extern crate edit_distance;

use std::collections::HashMap;
use std::fs::File;
use std::io::{BufRead, BufReader};
use edit_distance::edit_distance;

fn get_char_counts(input: &String) -> (bool, bool) {
    let mut has_two = false;
    let mut has_three = false;
    let mut map = HashMap::new();
    for char in input.chars() {
        map.entry(char).and_modify(|x| *x += 1).or_insert(1);
    }
    for (_char, count) in map {
        if has_two && has_three {
            break;
        }
        if count == 2 {
            has_two = true;
        } else if count == 3 {
            has_three = true;
        }
    }
    (has_two, has_three)
}

fn calc_checksum(lines: &Vec<String>) {
    let mut two_count = 0;
    let mut three_count = 0;
    for line in lines.iter() {
        let (has_two, has_three) = get_char_counts(line);
        if has_two {
            two_count += 1;
        }
        if has_three {
            three_count += 1;
        }
    }
    println!("checksum: {}", two_count * three_count);
}

fn find_single_char_difference(lines: &mut Vec<String>) {
    let line = lines.pop().expect("ran out of lines to find the single char difference!");
    let mut found = false;
    for other in lines.iter() {
        if edit_distance(&line, other) == 1 {
            found = true;
            println!("{} and {}", line, other);
            break;
        }
    }
    if !found {
        find_single_char_difference(lines);
    }
}

fn main() {
    let mut lines: Vec<String> = BufReader::new(File::open("input").unwrap())
        .lines()
        .map(|line| line.unwrap())
        .collect();
    calc_checksum(&lines);
    find_single_char_difference(&mut lines);
}
