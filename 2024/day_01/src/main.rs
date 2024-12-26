use itertools;
use itertools::Itertools;
use std::collections::HashMap;
use utils::*;

fn parse_to_lists(input: &Vec<String>) -> (Vec<i32>, Vec<i32>) {
    let left_list: Vec<i32> = input
        .iter()
        .map(|s| s.split("   ").nth(0).unwrap().parse::<i32>().unwrap())
        .sorted()
        .collect();
    let right_list: Vec<i32> = input
        .iter()
        .map(|s| s.split("   ").nth(1).unwrap().parse::<i32>().unwrap())
        .sorted()
        .collect();

    (left_list, right_list)
}

fn problem1(left_list: &Vec<i32>, right_list: &Vec<i32>) -> i32 {
    left_list
        .iter()
        .zip(right_list.iter())
        .map(|(l, r)| (l - r).abs())
        .sum::<i32>()
}

fn problem2(left_list: &Vec<i32>, right_list: &Vec<i32>) -> i32 {
    let mut freq_map: HashMap<i32, i32> = HashMap::new();

    right_list.iter().for_each(|&n| {
        *freq_map.entry(n).or_insert(0) += 1;
    });

    left_list
        .iter()
        .map(|n| match freq_map.get(n) {
            None => 0,
            Some(occ) => n * occ,
        })
        .sum::<i32>()
}

fn main() {
    let input = read_line_by_line("../../day_01/input/input.txt", '\n');
    let (left_list, right_list) = parse_to_lists(&input);
    let problem1 = problem1(&left_list, &right_list);
    let problem2 = problem2(&left_list, &right_list);

    println!("Part 1: {}", problem1);
    println!("Part 2: {}", problem2);
}
