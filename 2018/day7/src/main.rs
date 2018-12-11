use std::fs::File;
use std::io::{BufRead, BufReader};
use std::iter::Iterator;
use std::collections::{BTreeMap, HashMap};

type Day7Map = BTreeMap<char, Vec<char>>;

fn print_map(steps: &Day7Map) {
    for (key, value) in steps {
        print!("{}: ", key);
        for val in value.iter() {
            print!("{} ", val);
        }
        println!("");
    }
}

fn calc_order(steps: &mut Day7Map) -> String {
    let mut first_available = '_';
    for (key, values) in steps.iter_mut() {
        if values.is_empty() {
            values.push('_');
            first_available = *key;
            break;
        }
    };

    let mut final_result = String::from("");
    if first_available == '_' {
        final_result
    } else {
        for (_key, values) in steps.iter_mut() {
            values.retain(|&x| x != first_available);
        }
        final_result.push(first_available);
        final_result.push_str(calc_order(steps).as_str());
        final_result
    }
}

fn calc_time(steps: &mut Day7Map, time_elapsed: i64, workers: &mut HashMap<char, i64>) -> i64 {
    let mut available: Vec<char> = Vec::new();

    // decrement workers
    for (_key, time_remaining) in workers.iter_mut() {
        *time_remaining -= 1;
    }

    // remove finished workers
    workers.retain(|&k, &mut count| {
        if count == 0 {
            for (_key, values) in steps.iter_mut() {
                values.retain(|&x| x != k);
            }
            false
        } else {
            true
        }
    });

    let mut needed_workers = 5 - workers.len();
    let mut needs_work = false;
    for (key, values) in steps.iter_mut() {
        if values.is_empty() {
            needs_work = true;
            if needed_workers > 0 {
                values.push('_');
                available.push(*key);
                needed_workers -= 1;
                if needed_workers == 0 {
                    break;
                }
            }
        }
    }

    // return final sum of time
    if !needs_work && workers.is_empty() {
        return time_elapsed;
    }

    for new_step in available.iter() {
        // add new work
        workers.entry(*new_step).or_insert(*new_step as i64 - 4);
    }
    calc_time(steps, time_elapsed + 1, workers)
}

fn main() {
    let mut steps: Day7Map = BTreeMap::new();
    for line in BufReader::new(File::open("input").unwrap()).lines() {
        let line_str = line.unwrap();
        let mut chars = line_str.chars();
        let prereq = chars.nth(5).unwrap();
        let target = chars.nth(30).unwrap();
        steps.entry(target).and_modify(|e| e.push(prereq)).or_insert(vec![prereq]);
        steps.entry(prereq).or_insert(vec![]);
    };
    print_map(&steps);
    let order = calc_order(&mut steps.clone());
    println!("final order: {}", order);
    let time = calc_time(&mut steps.clone(), 0, &mut HashMap::<char, i64>::new());
    println!("time required for 5 workers: {}", time);
}
