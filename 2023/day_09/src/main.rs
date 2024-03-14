use itertools::Itertools;
use std::fs::read_to_string;

fn input_reader(path: String) -> String {
    read_to_string(path).unwrap()
}

fn sequence_routine(sequence: &Vec<i32>, variant: i32) -> i32 {
    let mut all_sequences: Vec<Vec<i32>> = Default::default();
    let mut curr_seq = sequence.clone();

    all_sequences.push(curr_seq.clone());
    loop {
        let sequences = curr_seq
            .iter()
            .tuple_windows()
            .map(|(x, y)| y - x)
            .collect::<Vec<i32>>();
        all_sequences.push(sequences.clone());
        curr_seq = sequences;
        if curr_seq.iter().filter(|x| **x == 0).count() == curr_seq.len() {
            break;
        }
    }
    return if variant == 1 {
        let last_elements: Vec<i32> = all_sequences.iter().map(|x| *x.last().unwrap()).collect();
        last_elements.iter().sum()
    } else {
        let first_elements: Vec<i32> = all_sequences.iter().map(|x| *x.first().unwrap()).collect();
        let modified_elements: Vec<i32> = first_elements
            .iter()
            .enumerate()
            .map(|(i, x)| if (i % 2) == 0 { *x } else { -*x })
            .collect();
        modified_elements.iter().sum()
    };
}

fn problem(part: i32) {
    let iterator = input_reader("./input/input.txt".to_string());
    let sequences: Vec<Vec<i32>> = iterator
        .split("\n")
        .map(|x| {
            x.trim()
                .split(" ")
                .map(|x| x.parse::<i32>().unwrap())
                .collect::<Vec<i32>>()
        })
        .collect();
    let result = sequences
        .iter()
        .map(|x| sequence_routine(&x, part))
        .collect::<Vec<i32>>()
        .iter()
        .sum::<i32>();
    println!("{:?}", result);
}

fn main() {
    problem(1);
    problem(2);
}
