use std::collections::{HashMap, HashSet};
use utils::*;

#[derive(Debug, Clone)]
struct Processor {
    registers: HashMap<char, i64>,
    instruction_pointer: usize,
}

fn parse_program(input: &Vec<String>) -> (Processor, Vec<i64>) {
    let mut processor: Processor = Processor {
        registers: HashMap::new(),
        instruction_pointer: 0,
    };
    let mut program: Vec<i64> = Vec::new();
    for line in input {
        if line.contains("Register ") {
            let content = line.split(" ").collect::<Vec<_>>();
            processor.registers.insert(
                content[1].chars().next().unwrap(),
                content[2].parse::<i64>().unwrap(),
            );
            continue;
        }
        if line.contains("Program: ") {
            let content = line.split(" ").collect::<Vec<_>>();
            program = content[1]
                .split(',')
                .collect::<Vec<_>>()
                .iter()
                .map(|x| x.parse::<i64>().unwrap())
                .collect::<Vec<i64>>()
        }
    }
    (processor, program)
}

fn read_operand(operand: &i64, processor: &Processor) -> i64 {
    match operand {
        0 | 1 | 2 | 3 => *operand,
        4 => *processor.registers.get(&'A').unwrap(),
        5 => *processor.registers.get(&'B').unwrap(),
        6 => *processor.registers.get(&'C').unwrap(),
        _ => -1,
    }
}

fn problem1(program: &Vec<i64>, processor: &mut Processor) -> String {
    let mut output: Vec<i64> = Vec::new();
    let mut jump = false;

    loop {
        if processor.instruction_pointer >= program.len() {
            break;
        }
        let opcode = program[processor.instruction_pointer];
        let operand = program[processor.instruction_pointer + 1];
        match opcode {
            0 => {
                let op1 = processor.registers.get(&'A').unwrap();
                let op2 = 2_i64.pow(read_operand(&operand, &processor) as u32);
                let result = op1 / op2;
                processor.registers.insert('A', result);
            }
            1 => {
                let op1 = processor.registers.get(&'B').unwrap();
                let op2 = operand;
                let result = op1 ^ op2;
                processor.registers.insert('B', result);
            }
            2 => {
                let result = read_operand(&operand, &processor) % 8;
                processor.registers.insert('B', result);
            }
            3 => {
                if *processor.registers.get(&'A').unwrap() != 0 {
                    processor.instruction_pointer = operand as usize;
                    jump = true;
                }
            }
            4 => {
                let op1 = processor.registers.get(&'B').unwrap();
                let op2 = processor.registers.get(&'C').unwrap();
                let result = op1 ^ op2;
                processor.registers.insert('B', result);
            }
            5 => {
                let result = read_operand(&operand, &processor) % 8;
                output.push(result);
            }
            6 => {
                let op1 = processor.registers.get(&'A').unwrap();
                let op2 = 2_i64.pow(read_operand(&operand, &processor) as u32);
                let result = op1 / op2;
                processor.registers.insert('B', result);
            }
            7 => {
                let op1 = processor.registers.get(&'A').unwrap();
                let op2 = 2_i64.pow(read_operand(&operand, &processor) as u32);
                let result = op1 / op2;
                processor.registers.insert('C', result);
            }
            _ => (),
        }
        if jump {
            jump = false
        } else {
            processor.instruction_pointer += 2;
        }
    }
    let mut result = String::new();
    for num in output.iter() {
        result.push_str(num.to_string().as_str());
        result.push(',')
    }
    result.pop();
    result
}

// The output is B % 8
// B depends on some operations done starting from A that is shifted 3 bits to the right after each operation
// A should be long enough to print 16 values and should be 0 at the 17th to avoid a jump

// We can bruteforce 3 bits at a time and reconstruct the value starting from the bottom
// By doing this we can find the leftmost digits to the rightmost

fn problem2(program: &Vec<i64>, processor: &mut Processor) -> i64 {
    let mut candidates: HashSet<i64> = HashSet::from([0]);
    // 16 groups of 3 bits each
    for i in 0..program.len() {
        let mut new_candidates: HashSet<i64> = HashSet::new();
        // Range of possible values
        for candidate in candidates.iter() {
            for j in 0b000..=0b111 {
                let test_value = (candidate << 3) | (j as i64);
                let mut temp_processor = processor.clone();
                temp_processor.registers.insert('A', test_value);
                let output = problem1(program, &mut temp_processor);
                let result: Vec<i64> = output
                    .chars()
                    .rev()
                    .filter(|c| *c != ',')
                    .map(|c| c.to_digit(10).unwrap() as i64)
                    .collect::<Vec<i64>>();
                let expected_value: Vec<i64> =
                    program.into_iter().rev().take(i + 1).cloned().collect();
                if result == expected_value {
                    new_candidates.insert(test_value);
                }
            }
        }
        candidates = new_candidates.clone();
    }
    *candidates.iter().min().unwrap()
}

fn main() {
    let input = read_line_by_line("../../day_17/input/input.txt", '\n');

    let (mut processor, program) = parse_program(&input);
    let problem1 = problem1(&program, &mut processor);
    println!("{:?}", problem1);

    let (mut processor, program) = parse_program(&input);
    let problem2 = problem2(&program, &mut processor);
    println!("{:?}", problem2);
}
