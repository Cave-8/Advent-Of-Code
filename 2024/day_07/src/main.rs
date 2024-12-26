use itertools::*;
use utils::*;

#[derive(Debug)]
struct Equation {
    test_val: u64,
    nums: Vec<u64>,
}

fn input_to_equations(input_string: &Vec<String>) -> Vec<Equation> {
    input_string
        .iter()
        .map(|x| {
            let purified_input = x.replace(":", "");
            let numbers = purified_input.split(" ").collect::<Vec<&str>>();
            let test_val = numbers[0].parse::<u64>().unwrap();
            let nums = numbers[1..]
                .to_vec()
                .iter()
                .map(|x| x.parse::<u64>().unwrap())
                .collect::<Vec<u64>>();
            Equation { test_val, nums }
        })
        .collect::<Vec<Equation>>()
}

fn add(a: u64, b: u64) -> u64 {
    a + b
}

fn multiply(a: u64, b: u64) -> u64 {
    a * b
}

fn concatenate(a: u64, b: u64) -> u64 {
    let digits = (b as f64).log(10.0).floor() as i32 + 1;
    a * (10u64.pow(digits as u32)) + b
}

fn problem(equations: &Vec<Equation>, part2: bool) -> u64 {
    let ops: Vec<fn(u64, u64) -> u64> = if !part2 {
        vec![add, multiply]
    } else {
        vec![add, multiply, concatenate]
    };
    equations
        .iter()
        .map(|equ| {
            // Find all combinations of operations over elements
            let ops_combinations: Vec<Vec<_>> = repeat_n(ops.iter().cloned(), equ.nums.len() - 1)
                .multi_cartesian_product()
                .collect();
            let mut mapped_value = 0;
            let mut update_flag = false;
            // Compute for all possible combinations eventually sum the result
            for op_seq in ops_combinations.iter() {
                let mut res = equ.nums[0];
                for (i, &x) in equ.nums.iter().skip(1).enumerate() {
                    let operation = &op_seq[i];
                    res = operation(res, x);
                }
                if res == equ.test_val && !update_flag {
                    mapped_value += res;
                    update_flag = true;
                }
            }
            mapped_value
        })
        .sum::<u64>()
}

fn main() {
    let input = read_line_by_line("../../day_07/input/input.txt", '\n');
    let equations = input_to_equations(&input);

    let problem1 = problem(&equations, false);
    println!("Problem 1: {:?}", problem1);
    let problem2 = problem(&equations, true);
    println!("Problem 2: {:?}", problem2);
}
