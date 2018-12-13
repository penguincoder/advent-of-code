use std::fs::File;
use std::io::{BufRead, BufReader};

fn get_coords_from_line(line: &String) -> (usize, usize, usize, usize) {
    let first_pieces: Vec<&str> = line.as_str().split(" @ ").collect();
    let _piece_number = first_pieces.get(0).unwrap();
    let piece_dims: Vec<&str> = first_pieces.get(1).unwrap().split(": ").collect();
    let top_left_pieces: Vec<&str> = piece_dims.get(0).unwrap().split(",").collect();
    let top_x = top_left_pieces.get(0).unwrap().parse::<usize>().unwrap();
    let top_y = top_left_pieces.get(1).unwrap().parse::<usize>().unwrap();
    let bottom_right_pieces: Vec<&str> = piece_dims.get(1).unwrap().split("x").collect();
    let bottom_x = top_x
        + bottom_right_pieces
            .get(0)
            .unwrap()
            .parse::<usize>()
            .unwrap();
    let bottom_y = top_y
        + bottom_right_pieces
            .get(1)
            .unwrap()
            .parse::<usize>()
            .unwrap();
    (top_x, top_y, bottom_x, bottom_y)
}

fn build_map(lines: &Vec<String>) -> [[u8; 1000]; 1000] {
    let mut map = [[0u8; 1000]; 1000];
    for line in lines.iter() {
        let (top_x, top_y, bottom_x, bottom_y) = get_coords_from_line(line);
        for i in top_x..bottom_x {
            for j in top_y..bottom_y {
                map[i][j] += 1;
            }
        }
    }
    map
}

fn calc_overlap(map: &[[u8; 1000]; 1000]) {
    let mut squares = 0;
    for row in map.iter() {
        for col in row.iter() {
            if *col > 1 {
                squares += 1;
            }
        }
    }
    println!("overlapped squares: {}", squares);
}

fn find_non_overlapped(lines: &Vec<String>, map: &[[u8; 1000]; 1000]) {
    for line in lines.iter() {
        let (top_x, top_y, bottom_x, bottom_y) = get_coords_from_line(line);
        let mut all_ones = true;
        for i in top_x..bottom_x {
            for j in top_y..bottom_y {
                if map[i][j] != 1 {
                    all_ones = false;
                    break;
                }
            }
            if !all_ones {
                break;
            }
        }
        if all_ones {
            println!("non overlapped line: {}", line);
            break;
        }
    }
}

fn main() {
    let lines: Vec<String> = BufReader::new(File::open("input").unwrap())
        .lines()
        .map(|line| line.unwrap())
        .collect();
    let map = build_map(&lines);
    calc_overlap(&map);
    find_non_overlapped(&lines, &map);
}
