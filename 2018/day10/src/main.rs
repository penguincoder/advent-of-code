extern crate regex;

use regex::Regex;
use std::collections::HashSet;
use std::fs::File;
use std::io::{BufRead, BufReader};
use std::iter::Iterator;

#[derive(Debug)]
struct Point {
    x: i32,
    y: i32,
    x_velocity: i32,
    y_velocity: i32
}

fn bounding_box(points: &Vec<Point>) -> (i32, i32, i32, i32) {
    let mut min_x = std::i32::MAX;
    let mut min_y = std::i32::MAX;
    let mut max_x = std::i32::MIN;
    let mut max_y = std::i32::MIN;

    for p in points.iter() {
        if min_x > p.x { min_x = p.x };
        if min_y > p.y { min_y = p.y };
        if max_x < p.x { max_x = p.x };
        if max_y < p.y { max_y = p.y };
    }

    (min_x, min_y, max_x, max_y)
}

fn tick(points: &mut Vec<Point>) -> i32 {
    let mut seconds = 0;
    let mut old_height = std::i32::MAX;

    loop {
        let (_min_x, min_y, _max_x, max_y) = bounding_box(points);
        let new_height = max_y - min_y;
        if new_height > old_height {
            for p in points.iter_mut() {
                p.x -= p.x_velocity;
                p.y -= p.y_velocity;
            }
            seconds -= 1;
            break;
        }

        old_height = new_height;
        seconds += 1;
        for p in points.iter_mut() {
            p.x += p.x_velocity;
            p.y += p.y_velocity;
        }
    }

    let (min_x, min_y, max_x, max_y) = bounding_box(points);
    println!("final bounding box: ({}, {}) ({}, {})", min_x, min_y, max_x, max_y);
    let mut final_set = HashSet::with_capacity(points.len());
    for p in points.iter() {
        final_set.insert((p.x, p.y));
    }

    for j in min_y..=max_y {
        for i in min_x..=max_x {
            if final_set.contains(&(i, j)) {
                print!("#")
            } else {
                print!(".")
            };
        }
        println!("");
    }

    seconds
}

fn main() {
    let lines: Vec<String> = BufReader::new(File::open("input").unwrap())
        .lines()
        .map(|line| line.unwrap())
        .collect();
    let mut points: Vec<Point> = Vec::with_capacity(lines.len());
    let re = Regex::new(r"^position=<([-| ]\d+), ([-| ]\d+)> velocity=<([-| ]\d), ([-| ]\d)>$").unwrap();
    for line in lines.iter() {
        if re.is_match(line) {
            let cap = re.captures(line).unwrap();
            let x = cap[1].trim_start().parse::<i32>().unwrap();
            let y = cap[2].trim_start().parse::<i32>().unwrap();
            let x_velocity = cap[3].trim_start().parse::<i32>().unwrap();
            let y_velocity = cap[4].trim_start().parse::<i32>().unwrap();
            points.push(Point {
                x,
                y,
                x_velocity,
                y_velocity
            });
        }
    }
    let seconds = tick(&mut points);
    println!("time elapsed: {}", seconds);
}
