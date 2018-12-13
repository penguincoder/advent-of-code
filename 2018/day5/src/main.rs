#[macro_use]
extern crate lazy_static;
extern crate regex;

use regex::Regex;
use std::fs::File;
use std::io::{BufRead, BufReader};
use std::iter::Iterator;

fn calc_remaining_monomers(polymers: &str) -> usize {
    lazy_static! {
        static ref POLYMER: Regex = Regex::new(r"aA|Aa|bB|Bb|cC|Cc|dD|Dd|eE|Ee|fF|Ff|gG|Gg|hH|Hh|iI|Ii|jJ|Jj|kK|Kk|lL|Ll|mM|Mm|nN|Nn|oO|Oo|pP|Pp|qQ|Qq|rR|Rr|sS|Ss|tT|Tt|uU|Uu|vV|Vv|wW|Ww|xX|Xx|yY|Yy|zZ|Zz").unwrap();
    }

    if POLYMER.is_match(polymers) {
        calc_remaining_monomers(&POLYMER.replace_all(polymers, ""))
    } else {
        polymers.len()
    }
}

fn calc_most_significant_monomer(polymers: &str) {
    let chars: Vec<char> = (b'a'..=b'z').map(char::from).collect();
    let mut min_letter = 'a';
    let mut min_length = polymers.len();
    for mychar in chars.iter() {
        let regex_str = format!(r"{}|{}", mychar, mychar.to_uppercase());
        let r = Regex::new(regex_str.as_str()).unwrap();
        let new_len = react_monomer(polymers, r);
        if new_len < min_length {
            min_letter = *mychar;
            min_length = new_len;
        }
    }
    println!(
        "minimum new length {} found by replacing {}",
        min_length, min_letter
    );
}

fn react_monomer(polymers: &str, r: Regex) -> usize {
    if r.is_match(polymers) {
        react_monomer(&r.replace_all(polymers, ""), r)
    } else {
        calc_remaining_monomers(polymers)
    }
}

fn main() {
    let lines: Vec<String> = BufReader::new(File::open("input").unwrap())
        .lines()
        .map(|line| line.unwrap())
        .collect();
    let polymers = lines.get(0).unwrap().as_str();
    let reacted_len = calc_remaining_monomers(polymers);
    println!("remaining polymers has size {}", reacted_len);
    calc_most_significant_monomer(polymers);
}
