extern crate regex;

use regex::Regex;
use std::collections::VecDeque;
use std::fs::File;
use std::io::{BufRead, BufReader};
use std::iter::Iterator;

fn play_game_deque(num_players: usize, max_marbles: u64) -> u64 {
    let mut game: VecDeque<u64> = VecDeque::with_capacity(max_marbles as usize);
    game.push_back(0);
    let mut scores = vec![0; num_players];

    for current_marble in 1..=max_marbles {
        if current_marble % 23 == 0 {
            for _ in 0..7 {
                let val = game.pop_back().unwrap();
                game.push_front(val);
            }
            let player = (current_marble as usize % num_players) as usize;
            let val = game.pop_back().unwrap();
            scores[player] += current_marble as u64 + val;
            let val2 = game.pop_front().unwrap();
            game.push_back(val2);
        } else {
            let val = game.pop_front().unwrap();
            game.push_back(val);
            game.push_back(current_marble);
        }
    }

    *scores.iter().max().unwrap()
}

fn main() {
    let lines: Vec<String> = BufReader::new(File::open("input").unwrap())
        .lines()
        .map(|line| line.unwrap())
        .collect();
    let re = Regex::new(r"^(\d+) players; last marble is worth (\d+) points").unwrap();
    for line in lines.iter() {
        if re.is_match(line) {
            let cap = re.captures(line).unwrap();
            let num_players = cap[1].parse::<usize>().unwrap();
            let max_marble = cap[2].parse::<u64>().unwrap();
            let score = play_game_deque(num_players, max_marble);
            println!("{}: {}", line, score);
            let new_max_marble = max_marble * 100;
            let score2 = play_game_deque(num_players, new_max_marble);
            println!("new_max_marble {}: score: {}", new_max_marble, score2);
        }
    }
}
