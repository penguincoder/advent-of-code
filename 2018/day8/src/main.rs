use std::fs::File;
use std::io::{BufRead, BufReader};
use std::iter::Iterator;

fn calc_metadata_sum(ints: &mut Vec<i32>) -> i32 {
    let subnode_count = ints.remove(0);
    let metadata_count = ints.remove(0);
    let mut sum = 0;
    for _i in 0..subnode_count {
        sum += calc_metadata_sum(ints);
    }
    for _i in 0..metadata_count {
        sum += ints.remove(0);
    }
    sum
}

fn calc_node_sum(ints: &mut Vec<i32>) -> i32 {
    let subnode_count = ints.remove(0);
    let metadata_count = ints.remove(0);

    let mut node_sums: Vec<i32> = Vec::with_capacity(subnode_count as usize);
    for _i in 0..subnode_count {
        node_sums.push(calc_node_sum(ints));
    }

    let mut metadatas: Vec<i32> = Vec::with_capacity(metadata_count as usize);
    for _i in 0..metadata_count {
        metadatas.push(ints.remove(0));
    }

    if subnode_count == 0 {
        metadatas.iter().sum()
    } else {
        let mut sum = 0;
        for m in metadatas.iter() {
            let index = (m - 1) as usize;
            if m <= &subnode_count {
                sum += node_sums[index];
            }
        }
        sum
    }
}

fn main() {
    let mut line = String::new();
    BufReader::new(File::open("input").unwrap())
        .read_line(&mut line)
        .unwrap();
    let len_withoutcrlf = line.trim_right().len();
    line.truncate(len_withoutcrlf);
    let ints: Vec<i32> = line
        .as_str()
        .split(" ")
        .map(|i| i.parse::<i32>().unwrap())
        .collect();
    let sum = calc_metadata_sum(&mut ints.clone());
    println!("part 1 sum is {}", sum);
    let sum2 = calc_node_sum(&mut ints.clone());
    println!("part 2 sum is {}", sum2);
}
