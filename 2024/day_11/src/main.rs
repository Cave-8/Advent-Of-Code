use std::collections::HashMap;
use utils::*;

fn find_digits(num: &u64) -> u64 {
    num.ilog(10.0 as u64) as u64 + 1
}

fn problem(stones: &Vec<(u64, u64)>, days: usize) -> u64 {
    let mut old_stones: HashMap<u64, u64> = HashMap::new();
    let mut new_stones: HashMap<u64, u64> = HashMap::new();

    for (stone, num_stones) in stones {
        old_stones.insert(*stone, *num_stones);
    }

    for _ in 0..days {
        new_stones = HashMap::new();
        for (stone, num_stone) in old_stones.iter() {
            if *stone == 0 {
                new_stones
                    .entry(1)
                    .and_modify(|x| *x += *num_stone)
                    .or_insert(*num_stone);
                continue;
            }

            if find_digits(&stone) % 2 == 0 {
                let stone_val = stone.to_string();
                let (left, right) = stone_val.split_at((find_digits(&stone) / 2) as usize);
                let (left_stone, right_stone) =
                    (left.parse::<u64>().unwrap(), right.parse::<u64>().unwrap());

                new_stones
                    .entry(left_stone)
                    .and_modify(|x| *x += num_stone)
                    .or_insert(*num_stone);
                new_stones
                    .entry(right_stone)
                    .and_modify(|x| *x += num_stone)
                    .or_insert(*num_stone);
                continue;
            }

            new_stones
                .entry(stone*2024)
                .and_modify(|x| *x += num_stone)
                .or_insert(*num_stone);
        }
        old_stones = new_stones.clone();
    }
    old_stones
        .iter()
        .filter(|(_, num)| **num > 0)
        .map(|(_, num_stones)| num_stones)
        .sum()
}

fn main() {
    let input = read_line_by_line("../../day_11/input/input.txt", '\n');
    let stones: Vec<(u64, u64)> = input[0]
        .split_whitespace()
        .map(|x| (x.parse::<u64>().unwrap(), 1))
        .collect();

    let problem1 = problem(&stones, 25);
    println!("Problem 1: {}", problem1);

    let problem2 = problem(&stones, 75);
    println!("Problem 2: {}", problem2);
}