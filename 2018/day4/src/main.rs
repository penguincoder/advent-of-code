#[macro_use]
extern crate lazy_static;
extern crate regex;

use regex::Regex;
use std::collections::HashMap;
use std::fs::File;
use std::io::{BufRead, BufReader};
use std::iter::Iterator;

type Day4Map = HashMap<u16, [u16; 60]>;

fn calc_minutes_from_lines(lines: &mut Vec<String>, map: &mut Day4Map, gid: u16, start: u16) {
    lazy_static! {
        static ref GUARD_RE: Regex =
            Regex::new(r"^\[\d{4}-\d{2}-\d{2} \d{2}:\d{2}\] Guard #(\d+) begins shift$").unwrap();
        static ref ASLEEP_RE: Regex =
            Regex::new(r"^\[\d{4}-\d{2}-\d{2} \d{2}:(\d{2})\] falls asleep$").unwrap();
        static ref AWAKE_RE: Regex =
            Regex::new(r"^\[\d{4}-\d{2}-\d{2} \d{2}:(\d{2})\] wakes up$").unwrap();
    }

    let line = lines.remove(0);
    let line = line.as_str();
    let guard_id = if GUARD_RE.is_match(line) {
        let caps = GUARD_RE.captures(line).unwrap();
        caps[1].parse::<u16>().unwrap()
    } else {
        gid
    };

    let start_minute = if ASLEEP_RE.is_match(line) {
        let caps = ASLEEP_RE.captures(line).unwrap();
        caps[1].parse::<u16>().unwrap()
    } else {
        start
    };

    if AWAKE_RE.is_match(line) {
        let caps = AWAKE_RE.captures(line).unwrap();
        let end_minute = caps[1].parse::<u16>().unwrap();
        map.entry(guard_id)
            .and_modify(|e| {
                for minute in (start_minute..end_minute).into_iter() {
                    e[usize::from(minute)] += 1;
                }
            })
            .or_insert([0u16; 60]);
    };

    if lines.is_empty() {
        return;
    } else {
        calc_minutes_from_lines(lines, map, guard_id, start_minute)
    }
}

fn get_max_from_map(map: &mut Day4Map) -> HashMap<u16, u16> {
    let mut count_map: HashMap<u16, u16> = HashMap::new();
    let mut max_gid = 0;
    let mut max_count = 0;
    let mut max_minute = 0;
    let mut most_max_gid = 0;
    let mut most_max_minute = 0;
    let mut most_max_minute_index = 0;

    for (key, value) in map {
        let sum = value.into_iter().sum();
        if sum > max_count {
            max_gid = *key;
            max_count = sum;
            let mut cur_max_min = 0;
            for i in 0..value.len() {
                if value[i] > cur_max_min {
                    cur_max_min = value[i];
                    max_minute = i as u16;
                }
            }
        }

        for i in 0..value.len() {
            if value[i] > most_max_minute {
                most_max_minute = value[i];
                most_max_minute_index = i as u16;
                most_max_gid = *key;
            }
        }

        count_map.insert(*key, sum);
    }
    println!(
        "max guard id is {} sum {}, max minute of {} with val of {}",
        max_gid,
        max_count,
        max_minute,
        max_gid * max_minute
    );
    println!(
        "guard {} most asleep on a single minute {} with a count of {} is {}",
        most_max_gid,
        most_max_minute_index,
        most_max_minute,
        most_max_gid * most_max_minute_index
    );
    count_map
}

fn main() {
    let mut lines: Vec<String> = BufReader::new(File::open("sorted-input").unwrap())
        .lines()
        .map(|line| line.unwrap())
        .collect();
    let mut map = Day4Map::new();
    calc_minutes_from_lines(&mut lines, &mut map, 0, 0);
    /*for (key, value) in &map {
        print!("{}: ", key);
        for r in value.iter() {
            print!("{} ", r);
        }
        println!();
    }*/
    get_max_from_map(&mut map);
}
