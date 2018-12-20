use std::fs::File;
use std::io::{BufRead, BufReader};

fn mutate_chocolate(scores: &mut Vec<u8>, recipes: i32) -> (String, usize) {
    let mut elf1: usize = 0;
    let mut elf2: usize = 1;
    loop {
        let elf1score = scores[elf1];
        let elf2score = scores[elf2];
        let sum = elf1score + elf2score;
        let sum_str = format!("{}", sum);
        for c in sum_str.chars() {
            scores.push(c as u8 - 48);
        }

        if scores.len() > recipes as usize + 100000000 {
            break;
        }

        elf1 = (elf1 + (1 + elf1score as usize)) % scores.len();
        elf2 = (elf2 + (1 + elf2score as usize)) % scores.len();
    }

    let mut result = String::new();
    for i in 0..10 {
        result.push((scores[recipes as usize + i] + 48) as char);
    }

    let mut score_str = String::with_capacity(scores.len());
    for s in scores.iter() {
        score_str.push((s + 48) as char);
    }
    //println!("{}", score_str);
    let recipe_str = format!("{}", recipes);
    let recipe_index = score_str.find(recipe_str.as_str()).unwrap();

    (result, recipe_index)
}

fn main() {
    let mut line = String::new();
    BufReader::new(File::open("input").unwrap())
        .read_line(&mut line)
        .unwrap();
    let recipes = line.trim().parse::<i32>().unwrap();
    let mut scores: Vec<u8> = Vec::with_capacity(1000000);
    scores.push(3);
    scores.push(7);
    let (final_score, final_index) = mutate_chocolate(&mut scores.clone(), recipes);
    println!("part 1 final score {}", final_score);
    println!("recipe repeated at index {}", final_index);
}
