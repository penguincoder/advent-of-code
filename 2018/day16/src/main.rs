#[macro_use]
extern crate log;
extern crate pretty_env_logger;

use std::collections::{HashMap, HashSet};
use std::fs::File;
use std::io::{BufRead, BufReader};

type Registers = [u64; 4];
type Instruction = [u64; 4];
type DecodeTable = HashMap<u64, String>;

struct OpSample {
    before: Registers,
    instruction: Instruction,
    after: Registers,
}

fn extract_values_from_line(line: String) -> Registers {
    let reg_chars = line.as_str();
    let (_, second) = reg_chars.split_at(9);
    let third = second.replace(",", "").replace("]", "");
    iter_to_registers(third.split_whitespace())
}

fn iter_to_registers(iter: std::str::SplitWhitespace) -> Registers {
    let mut results: Registers = [0u64; 4];
    for (index, r) in iter.enumerate() {
        results[index] = r.parse::<u64>().unwrap();
    }
    results
}

fn read_samples_from_file(filename: &str) -> Vec<OpSample> {
    let mut lines: Vec<String> = BufReader::new(File::open(filename).unwrap())
        .lines()
        .map(|line| line.unwrap())
        .collect();
    let mut samples: Vec<OpSample> = vec![];
    while !lines.is_empty() {
        let before = extract_values_from_line(lines.remove(0));
        let instruction = iter_to_registers(lines.remove(0).split_whitespace());
        let after = extract_values_from_line(lines.remove(0));
        lines.remove(0);
        samples.push(OpSample {
            before,
            instruction,
            after
        });
    }
    samples
}

fn read_instructions_from_file(filename: &str) -> Vec<Instruction> {
    let lines: Vec<Instruction> = BufReader::new(File::open(filename).unwrap())
        .lines()
        .map(|line| iter_to_registers(line.unwrap().split_whitespace()))
        .collect();
    lines
}

fn addr(input_registers: Registers, instruction: Instruction) -> Registers {
    let mut output_registers = input_registers.clone();
    output_registers[instruction[3] as usize] = input_registers[instruction[1] as usize] + input_registers[instruction[2] as usize];
    debug!("addr: input {:?} output {:?}", input_registers, output_registers);
    output_registers
}

fn addi(input_registers: Registers, instruction: Instruction) -> Registers {
    let mut output_registers = input_registers.clone();
    output_registers[instruction[3] as usize] = input_registers[instruction[1] as usize] + instruction[2];
    debug!("addi: input {:?} output {:?}", input_registers, output_registers);
    output_registers
}

fn mulr(input_registers: Registers, instruction: Instruction) -> Registers {
    let mut output_registers = input_registers.clone();
    output_registers[instruction[3] as usize] = input_registers[instruction[1] as usize] * input_registers[instruction[2] as usize];
    debug!("mulr: input {:?} output {:?}", input_registers, output_registers);
    output_registers
}

fn muli(input_registers: Registers, instruction: Instruction) -> Registers {
    let mut output_registers = input_registers.clone();
    output_registers[instruction[3] as usize] = input_registers[instruction[1] as usize] * instruction[2];
    debug!("muli: input {:?} output {:?}", input_registers, output_registers);
    output_registers
}

fn banr(input_registers: Registers, instruction: Instruction) -> Registers {
    let mut output_registers = input_registers.clone();
    output_registers[instruction[3] as usize] = input_registers[instruction[1] as usize] & input_registers[instruction[2] as usize];
    debug!("banr: input {:?} output {:?}", input_registers, output_registers);
    output_registers
}

fn bani(input_registers: Registers, instruction: Instruction) -> Registers {
    let mut output_registers = input_registers.clone();
    output_registers[instruction[3] as usize] = input_registers[instruction[1] as usize] & instruction[2];
    debug!("bani: input {:?} output {:?}", input_registers, output_registers);
    output_registers
}

fn borr(input_registers: Registers, instruction: Instruction) -> Registers {
    let mut output_registers = input_registers.clone();
    output_registers[instruction[3] as usize] = input_registers[instruction[1] as usize] | input_registers[instruction[2] as usize];
    debug!("borr: input {:?} output {:?}", input_registers, output_registers);
    output_registers
}

fn bori(input_registers: Registers, instruction: Instruction) -> Registers {
    let mut output_registers = input_registers.clone();
    output_registers[instruction[3] as usize] = input_registers[instruction[1] as usize] | instruction[2];
    debug!("bori: input {:?} output {:?}", input_registers, output_registers);
    output_registers
}

fn setr(input_registers: Registers, instruction: Instruction) -> Registers {
    let mut output_registers = input_registers.clone();
    output_registers[instruction[3] as usize] = input_registers[instruction[1] as usize];
    debug!("setr: input {:?} output {:?}", input_registers, output_registers);
    output_registers
}

fn seti(input_registers: Registers, instruction: Instruction) -> Registers {
    let mut output_registers = input_registers.clone();
    output_registers[instruction[3] as usize] = instruction[1];
    debug!("seti: input {:?} output {:?}", input_registers, output_registers);
    output_registers
}

fn gtir(input_registers: Registers, instruction: Instruction) -> Registers {
    let mut output_registers = input_registers.clone();
    if instruction[1] > input_registers[instruction[2] as usize] {
        output_registers[instruction[3] as usize] = 1;
    } else {
        output_registers[instruction[3] as usize] = 0;
    }
    debug!("gtir: input {:?} output {:?}", input_registers, output_registers);
    output_registers
}

fn gtri(input_registers: Registers, instruction: Instruction) -> Registers {
    let mut output_registers = input_registers.clone();
    if input_registers[instruction[1] as usize] > instruction[2] {
        output_registers[instruction[3] as usize] = 1;
    } else {
        output_registers[instruction[3] as usize] = 0;
    }
    debug!("gtri: input {:?} output {:?}", input_registers, output_registers);
    output_registers
}

fn gtrr(input_registers: Registers, instruction: Instruction) -> Registers {
    let mut output_registers = input_registers.clone();
    if input_registers[instruction[1] as usize] > input_registers[instruction[2] as usize] {
        output_registers[instruction[3] as usize] = 1;
    } else {
        output_registers[instruction[3] as usize] = 0;
    }
    debug!("gtrr: input {:?} output {:?}", input_registers, output_registers);
    output_registers
}

fn eqir(input_registers: Registers, instruction: Instruction) -> Registers {
    let mut output_registers = input_registers.clone();
    if instruction[1] == input_registers[instruction[2] as usize] {
        output_registers[instruction[3] as usize] = 1;
    } else {
        output_registers[instruction[3] as usize] = 0;
    }
    debug!("eqir: input {:?} output {:?}", input_registers, output_registers);
    output_registers
}

fn eqri(input_registers: Registers, instruction: Instruction) -> Registers {
    let mut output_registers = input_registers.clone();
    if input_registers[instruction[1] as usize] == instruction[2] {
        output_registers[instruction[3] as usize] = 1;
    } else {
        output_registers[instruction[3] as usize] = 0;
    }
    debug!("eqri: input {:?} output {:?}", input_registers, output_registers);
    output_registers
}

fn eqrr(input_registers: Registers, instruction: Instruction) -> Registers {
    let mut output_registers = input_registers.clone();
    if input_registers[instruction[1] as usize] == input_registers[instruction[2] as usize] {
        output_registers[instruction[3] as usize] = 1;
    } else {
        output_registers[instruction[3] as usize] = 0;
    }
    debug!("eqrr: input {:?} output {:?}", input_registers, output_registers);
    output_registers
}

fn matching_opcodes(i: &OpSample) -> Vec<&str> {
    let mut m: Vec<&str> = Vec::with_capacity(16);
    if addr(i.before, i.instruction) == i.after {
        debug!("{:?} matches addr", i.instruction);
        m.push("addr");
    }
    if addi(i.before, i.instruction) == i.after {
        debug!("{:?} matches addi", i.instruction);
        m.push("addi");
    }
    if mulr(i.before, i.instruction) == i.after {
        debug!("{:?} matches mulr", i.instruction);
        m.push("mulr");
    }
    if muli(i.before, i.instruction) == i.after {
        debug!("{:?} matches muli", i.instruction);
        m.push("muli");
    }
    if banr(i.before, i.instruction) == i.after {
        debug!("{:?} matches banr", i.instruction);
        m.push("banr");
    }
    if bani(i.before, i.instruction) == i.after {
        debug!("{:?} matches bani", i.instruction);
        m.push("bani");
    }
    if borr(i.before, i.instruction) == i.after {
        debug!("{:?} matches borr", i.instruction);
        m.push("borr");
    }
    if bori(i.before, i.instruction) == i.after {
        debug!("{:?} matches bori", i.instruction);
        m.push("bori");
    }
    if setr(i.before, i.instruction) == i.after {
        debug!("{:?} matches setr", i.instruction);
        m.push("setr");
    }
    if seti(i.before, i.instruction) == i.after {
        debug!("{:?} matches seti", i.instruction);
        m.push("seti");
    }
    if gtir(i.before, i.instruction) == i.after {
        debug!("{:?} matches gtir", i.instruction);
        m.push("gtir");
    }
    if gtri(i.before, i.instruction) == i.after {
        debug!("{:?} matches gtri", i.instruction);
        m.push("gtri");
    }
    if gtrr(i.before, i.instruction) == i.after {
        debug!("{:?} matches gtrr", i.instruction);
        m.push("gtrr");
    }
    if eqir(i.before, i.instruction) == i.after {
        debug!("{:?} matches eqir", i.instruction);
        m.push("eqir");
    }
    if eqri(i.before, i.instruction) == i.after {
        debug!("{:?} matches eqri", i.instruction);
        m.push("eqri");
    }
    if eqrr(i.before, i.instruction) == i.after {
        debug!("{:?} matches eqrr", i.instruction);
        m.push("eqrr");
    }
    m
}

fn reduce_decode_map(map: &mut HashMap<u64, HashSet<&str>>) -> DecodeTable {
    let mut results: DecodeTable = HashMap::new();
    let mut removed_one = true;
    while !map.is_empty() && removed_one {
        removed_one = false;
        let mut this_pass_found_opcodes: HashSet<u64> = HashSet::new();
        let mut this_pass_found_instr: HashSet<&str> = HashSet::new();
        for (confirmed_op, confirmed_instructions) in map.iter() {
            if confirmed_instructions.len() > 1 {
                continue;
            }
            removed_one = true;
            let confirmed_instruction = confirmed_instructions.iter().nth(0).unwrap();
            results.entry(*confirmed_op).or_insert(confirmed_instruction.to_string());
            this_pass_found_opcodes.insert(*confirmed_op);
            this_pass_found_instr.insert(*confirmed_instruction);
        }
        for op in this_pass_found_opcodes.iter() {
            map.retain(|k,_| k != op);
        }
        for instr in this_pass_found_instr.iter() {
            for (_k,v) in map.iter_mut() {
                v.retain(|i| i != instr);
            }
        }
    }
    results
}

fn opcodes_part_one(samples: &Vec<OpSample>) -> (u16, DecodeTable) {
    let mut i: u16 = 0;
    let mut decode_map: HashMap<u64, HashSet<&str>> = HashMap::new();
    for sample in samples.iter() {
        let matched_ops = matching_opcodes(&sample);
        if matched_ops.len() >= 3 {
            i += 1;
        }
        debug!("{:?} matches {} opcodes", sample.instruction, matched_ops.len());
        let instruction = sample.instruction[0];
        for op in matched_ops.iter() {
            decode_map.entry(instruction).and_modify(|m| {
                m.insert(op);
            }).or_insert(HashSet::<&str>::new());
        }
    }
    let final_decode_map = reduce_decode_map(&mut decode_map);
    for (op, instr) in final_decode_map.iter() {
        debug!("final decode of {} into {}", op, instr);
    }
    (i, final_decode_map)
}

fn execute_program(decode_table: &DecodeTable, instructions: &Vec<Instruction>) -> Registers {
    let mut registers: Registers = [0u64; 4];
    for instruction in instructions.iter() {
        debug!("{:?}", instruction);
        registers = match decode_table.get(&instruction[0]) {
            Some(x) => {
                match x.as_str() {
                    "addr" => addr(registers, *instruction),
                    "addi" => addi(registers, *instruction),
                    "mulr" => mulr(registers, *instruction),
                    "muli" => muli(registers, *instruction),
                    "banr" => banr(registers, *instruction),
                    "bani" => bani(registers, *instruction),
                    "borr" => borr(registers, *instruction),
                    "bori" => bori(registers, *instruction),
                    "setr" => setr(registers, *instruction),
                    "seti" => seti(registers, *instruction),
                    "gtir" => gtir(registers, *instruction),
                    "gtri" => gtri(registers, *instruction),
                    "gtrr" => gtrr(registers, *instruction),
                    "eqir" => eqir(registers, *instruction),
                    "eqri" => eqri(registers, *instruction),
                    "eqrr" => eqrr(registers, *instruction),
                    _ => panic!("No decoded instruction available for {}", x),
                }
            },
            None => panic!("Unmapped instruction!"),
        }
    }
    registers
}

fn main() {
    pretty_env_logger::init();
    let samples = read_samples_from_file("input-part-1");
    debug!("samples len is {}", samples.len());
    let (part1, decode_table) = opcodes_part_one(&samples);
    println!("samples matching 3 or more opcodes: {}", part1);
    let program = read_instructions_from_file("input-part-2");
    let registers: Registers = execute_program(&decode_table, &program);
    println!("final registers: {:?}", registers);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_sample_one() {
        let samples = read_samples_from_file("test-input");
        assert_eq!(matching_opcodes(&samples[0]), 3);
    }
}
