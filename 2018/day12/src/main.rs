use std::collections::HashMap;
use std::fs::File;
use std::io::{BufRead, BufReader};
use std::iter::Iterator;

const MAP_SIZE: usize = 500;
const MAP_ZERO_INDEX: usize = 200;

fn get_score(state: &[char; MAP_SIZE]) -> i64 {
    let mut score = 0;
    for i in 0..MAP_SIZE {
        if state[i] == '#' {
            score += (i - MAP_ZERO_INDEX) as i64;
        }
    }
    score
}

fn run_game(
    initial_state: String,
    states: HashMap<String, char>,
    generations: i64,
) -> (i64, [i64; MAP_SIZE]) {
    let mut last_state = ['.'; MAP_SIZE];
    for (i, c) in initial_state.char_indices() {
        if c == '#' {
            last_state[MAP_ZERO_INDEX + i] = '#';
        }
    }

    let mut scores = [0i64; MAP_SIZE];

    for gen in 0..generations {
        let mut next_state = ['.'; MAP_SIZE];
        for i in 2..(MAP_SIZE - 2) {
            let mut key = String::new();
            for j in i - 2..=i + 2 {
                key.push(last_state[j]);
            }
            if let Some(dest_state) = states.get(&key) {
                next_state[i] = *dest_state;
            }
        }
        last_state = next_state;
        scores[gen as usize] = get_score(&last_state);
    }

    (scores[generations as usize - 1], scores)
}

fn find_big_score(initial_state: String, states: HashMap<String, char>, iterations: i64) -> i64 {
    let (_, scores) = run_game(initial_state, states, MAP_SIZE as i64);
    for i in 0..(MAP_SIZE - 3) {
        let diff1 = scores[i + 1] - scores[i];
        let diff2 = scores[i + 2] - scores[i + 1];
        let diff3 = scores[i + 3] - scores[i + 2];
        if diff1 == diff2 {
            if diff2 == diff3 {
                let sum = ((iterations - i as i64 - 1) * diff1) + scores[i];
                println!(
                    "({} - {}) * {} + {} = {}",
                    iterations - 1,
                    i as i64,
                    diff1,
                    scores[i],
                    sum
                );
                return sum;
            }
        }
    }

    -1
}

fn main() {
    let mut lines: Vec<String> = BufReader::new(File::open("input").unwrap())
        .lines()
        .map(|line| line.unwrap())
        .collect();

    let mut default_state = lines.remove(0);
    default_state.replace_range(0..15, "");
    lines.remove(0);
    let mut states: HashMap<String, char> = HashMap::new();
    for line in lines.iter() {
        let mut chars = line.chars();
        let mut state = String::with_capacity(5);
        state.push(chars.nth(0).unwrap());
        state.push(chars.nth(0).unwrap());
        state.push(chars.nth(0).unwrap());
        state.push(chars.nth(0).unwrap());
        state.push(chars.nth(0).unwrap());
        let dest = chars.nth(4).unwrap();
        states.insert(state, dest);
    }

    let (part1score, _) = run_game(default_state.clone(), states.clone(), 20);
    println!("part 1 score after 20 generations: {}", part1score);
    let iterations = 50000000000;
    let part2score = find_big_score(default_state, states, iterations);
    println!("part 2 score after {} is {}", iterations, part2score);
}
