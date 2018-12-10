use std::fs::File;
use std::io::{BufRead, BufReader};
use std::iter::Iterator;
use std::collections::HashMap;

struct Point {
    x: usize,
    y: usize,
}

impl Point {
    fn distance(&self, other: &Point) -> i64 {
        ((self.x - other.x) as i64).abs() + ((self.y - other.y) as i64).abs()
    }
}

impl std::fmt::Display for Point {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "({}, {})", self.x, self.y)
    }
}

fn calc_max_area(points: &Vec<Point>, min_x: usize, min_y: usize, max_x: usize, max_y: usize) -> (u8, usize) {
    let distances = calc_distances(points, min_x, min_y, max_x, max_y);
    let mut areas: HashMap<u8, usize> = HashMap::new();
    for i in min_x..=max_x {
        for j in min_y..=max_y {
            if distances[i][j] == 0 {
                continue;
            }
            areas.entry(distances[i][j]).and_modify(|e| *e += 1).or_insert(1);
        }
    }
    let mut index = 0;
    let mut max_area = 0;
    let mut max_area_index = 0;
    for _point in points.iter() {
        index += 1;
        let cur_area = areas.get(&index).unwrap();
        if cur_area > &max_area {
            max_area = *cur_area;
            max_area_index = index;
        }
    }
    for (key, value) in areas.iter() {
        println!("{}: {} area {}", key, points.get((index - 1) as usize).unwrap(), value);
    }
    (max_area_index, max_area)
}

fn calc_distances(points: &Vec<Point>, min_x: usize, min_y: usize, max_x: usize, max_y: usize) -> [[u8; 500]; 500] {
    let mut distances = [[0u8; 500]; 500];
    for i in min_x..=max_x {
        for j in min_y..=max_y {
            let cur_point = Point {
                x: i,
                y: j
            };
            let mut min_dis = 500;
            let mut found_dupe = false;
            let mut min_index = 0;
            let mut index = 0;
            for point in points.iter() {
                index += 1;
                let dis = cur_point.distance(point);
                if dis < min_dis {
                    min_dis = dis;
                    found_dupe = false;
                    min_index = index;
                } else if dis == min_dis {
                    found_dupe = true;
                }
            }
            if !found_dupe {
                distances[i][j] = min_index;
            }
        }
    }
    distances
}

fn calc_safe_zone(points: &Vec<Point>, threshold: i64) -> i64 {
    let mut area = 0;
    for i in 0..500 {
        for j in 0..500 {
            let point = Point { x: i, y: j };
            let distance: i64 = points.iter().map(|p| p.distance(&point)).sum();
            if distance < threshold {
                area += 1;
            }
        }
    }
    area
}

fn main() {
    let points: Vec<Point> = BufReader::new(File::open("input").unwrap())
        .lines()
        .map(|line| {
            let points_str = line.unwrap();
            let points_ints: Vec<usize> = points_str
                .as_str()
                .split(", ")
                .map(|p| p.parse::<usize>().unwrap())
                .collect();
            Point {
                x: *points_ints.get(0).unwrap(),
                y: *points_ints.get(1).unwrap(),
            }
        })
        .collect();
    let mut min_x = 500;
    let mut min_y = 500;
    let mut max_x = 0;
    let mut max_y = 0;
    for point in points.iter() {
        if point.x < min_x { min_x = point.x };
        if point.x > max_x { max_x = point.x };
        if point.y < min_y { min_y = point.y };
        if point.y > max_y { max_y = point.y };
    }
    let width = max_x - min_x;
    let height = max_y - min_y;
    println!("map size {}x{}", width, height);
    let (max_area_point, max_area) = calc_max_area(&points, min_x, min_y, max_x, max_y);
    let max_area_point_index = (max_area_point - 1) as usize;
    println!("max area point {} {} is {}", max_area_point, points.get(max_area_point_index).unwrap(), max_area);
    let safe_zone = calc_safe_zone(&points, 10000);
    println!("safe zone area {}", safe_zone);
}
