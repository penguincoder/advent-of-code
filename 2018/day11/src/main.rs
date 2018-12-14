use std::fs::File;
use std::io::{BufRead, BufReader};
use std::iter::Iterator;

fn calc_map_from_serial_number(serial_number: i32) -> [[i32; 300]; 300] {
    let mut result = [[0i32; 300]; 300];
    for i in 0..300 {
        for j in 0..300 {
            let rack_id = i + 10;
            let power = ((rack_id * j) + serial_number) * rack_id;
            let hundreds = power.to_string().as_str().chars().rev().nth(2).unwrap();
            result[i as usize][j as usize] = (hundreds as i32) - 48 - 5;
        }
    }
    result
}

fn find_max_sum(map: &[[i32; 300]; 300], subsize: usize) -> (usize, usize, i32) {
    let presums = precalculate_sums(&map, subsize);
    let mut x = 0;
    let mut y = 0;
    let mut sum = 0;
    for i in 0..(300 - subsize) {
        for j in 0..(300 - subsize) {
            let mut new_sum = 0;
            for s in 0..subsize {
                new_sum += presums[i + s][j];
            }
            if new_sum > sum {
                sum = new_sum;
                x = i;
                y = j;
            }
        }
    }

    (x, y, sum)
}

fn precalculate_sums(map: &[[i32; 300]; 300], subsize: usize) -> [[i32; 300]; 300] {
    let mut result = [[0i32; 300]; 300];
    for i in 0..300 {
        for j in 0..(300 - subsize) {
            for s in 0..subsize {
                result[i][j] += map[i][j + s];
            }
        }
    }

    result
}

fn find_variable_max_sum(map: &[[i32; 300]; 300]) -> (usize, usize, i32, usize) {
    let mut x = 0;
    let mut y = 0;
    let mut size = 0;
    let mut sum = 0;
    for i in 1..=300 {
        let (new_x, new_y, new_sum) = find_max_sum(map, i);
        if new_sum == 0 { break; }
        if new_sum > sum {
            size = i;
            sum = new_sum;
            x = new_x;
            y = new_y;
        }
    }

    (x, y, sum, size)
}

fn main() {
    let mut line = String::new();
    BufReader::new(File::open("input").unwrap())
        .read_line(&mut line)
        .unwrap();
    let serial_number = line.trim().parse::<i32>().unwrap();

    let map = calc_map_from_serial_number(serial_number);
    let (x, y, sum) = find_max_sum(&map, 3);
    println!("maximum sum coordinate of ({}, {}) with sum of {}", x, y, sum);

    let (x, y, sum, size) = find_variable_max_sum(&map);
    println!("maximum variable sum found at ({}, {}) sum {} with size of {}", x, y, sum, size);
}
