use std::fs::File;
use std::io::{BufRead, BufReader};

const MAP_SIZE: usize = 150;

#[derive(Debug, Clone)]
struct Cart {
    x: usize,
    y: usize,
    x_delta: i8,
    y_delta: i8,
    intersection: u8,
    removed: bool
}

impl Cart {
    pub fn turn_right(&mut self) {
        if self.x_delta == 0 {
            if self.y_delta == 1 {
                self.x_delta = -1;
            } else {
                self.x_delta = 1;
            }
            self.y_delta = 0;
        } else {
            if self.x_delta == 1 {
                self.y_delta = 1;
            } else {
                self.y_delta = -1;
            }
            self.x_delta = 0;
        }
    }

    pub fn turn_left(&mut self) {
        if self.x_delta == 0 {
            if self.y_delta == 1 {
                self.x_delta = 1;
            } else {
                self.x_delta = -1;
            }
            self.y_delta = 0;
        } else {
            if self.x_delta == 1 {
                self.y_delta = -1;
            } else {
                self.y_delta = 1;
            }
            self.x_delta = 0;
        }
    }

    pub fn change_direction_at_intersection(&mut self) {
        match self.intersection {
            0 => self.turn_left(),
            2 => self.turn_right(),
            _ => {}
        }
        self.intersection = (self.intersection + 1) % 3;
    }
}

fn find_collision(map: &[[char; MAP_SIZE]; MAP_SIZE], carts: &mut Vec<Cart>, find_last: bool) -> (usize, usize, u32) {
    let mut ticks = 0;
    let cart_len = carts.len();
    loop {
        carts.sort_by(|a, b| {
            if a.y == b.y {
                a.x.cmp(&b.x)
            } else {
                a.y.cmp(&b.y)
            }
        });

        for i in 0..cart_len {
            if carts[i].removed {
                continue;
            }
            let next_x = carts[i].x + carts[i].x_delta as usize;
            let next_y = carts[i].y + carts[i].y_delta as usize;

            for j in 0..cart_len {
                if i != j && !carts[j].removed && carts[j].x == next_x && carts[j].y == next_y {
                    if find_last {
                        carts[j].removed = true;
                        carts[i].removed = true;
                    } else {
                        return (next_x, next_y, ticks);
                    }
                }
            }

            match map[next_x][next_y] {
                '/' => {
                    if carts[i].x_delta == 0 {
                        carts[i].turn_right();
                    } else {
                        carts[i].turn_left();
                    }
                },
                '\\' => {
                    if carts[i].x_delta == 0 {
                        carts[i].turn_left();
                    } else {
                        carts[i].turn_right();
                    }
                },
                '+' => {
                    carts[i].change_direction_at_intersection();
                },
                ' ' => {
                    panic!("off the rails!");
                },
                _ => {}
            }
            carts[i].x = next_x;
            carts[i].y = next_y;

            if find_last {
                let mut remaining_count = 0;
                let mut remaining_index = 0;
                for j in 0..cart_len {
                    if carts[j].removed == false {
                        remaining_count += 1;
                        remaining_index = j;
                    }
                }
                if remaining_count == 1 {
                    return (carts[remaining_index].x, carts[remaining_index].y, ticks);
                }
            }
        }
        ticks += 1; // gross
    }
}

fn main() {
    let lines: Vec<String> = BufReader::new(File::open("input").unwrap())
        .lines()
        .map(|line| line.unwrap())
        .collect();
    let mut map = [[' '; MAP_SIZE]; MAP_SIZE];
    let mut carts: Vec<Cart> = Vec::with_capacity(20);
    let mut line_number = 0;
    for line in lines.iter() {
        for (j, c) in line.char_indices() {
            if c == 'v' {
                carts.push(Cart {
                    x: j,
                    y: line_number,
                    x_delta: 0,
                    y_delta: 1,
                    intersection: 0,
                    removed: false
                });
                map[j][line_number] = '|';
            } else if c == '^' {
                carts.push(Cart {
                    x: j,
                    y: line_number,
                    x_delta: 0,
                    y_delta: -1,
                    intersection: 0,
                    removed: false
                });
                map[j][line_number] = '|';
            } else if c == '<' {
                carts.push(Cart {
                    x: j,
                    y: line_number,
                    x_delta: -1,
                    y_delta: 0,
                    intersection: 0,
                    removed: false
                });
                map[j][line_number] = '-';
            } else if c == '>' {
                carts.push(Cart {
                    x: j,
                    y: line_number,
                    x_delta: 1,
                    y_delta: 0,
                    intersection: 0,
                    removed: false
                });
                map[j][line_number] = '-';
            } else {
                map[j][line_number] = c;
            }
        }
        line_number += 1;
    }
    let (x, y, ticks) = find_collision(&map, &mut carts.clone(), false);
    println!("found collision at {},{} after {} ticks", x, y, ticks);
    let (x, y, ticks) = find_collision(&map, &mut carts.clone(), true);
    println!("found last remaining cart at {},{} after {} ticks", x, y, ticks);
}
