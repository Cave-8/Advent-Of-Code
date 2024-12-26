use std::collections::{HashSet};
use utils::read_line_by_line;

fn parse_input(input: &Vec<String>) -> (Vec<String>, HashSet<String>) {
    let mut patterns: Vec<String> = Vec::new();
    let mut towels: HashSet<String> = HashSet::new();

    towels = input[0].split(", ").map(|s| s.to_string()).collect();
    for line in input {
        if line.len() > 2 && !line.contains(',') {
            patterns.push(line.trim().to_string());
        }
    }

    (patterns, towels)
}

fn can_build_pattern(pattern: String, towels: &HashSet<String>) -> usize {
    let mut ways_of_building_substrings = vec![0; pattern.len() + 1];
    // Empty word always buildable
    ways_of_building_substrings[0] = 1;
    for i in 1..=pattern.len() {
        for j in 0..i {
            // Add a new way to build the word
            if ways_of_building_substrings[j] > 0 && towels.contains(&pattern[j..i]) {
                ways_of_building_substrings[i] += ways_of_building_substrings[j];
            }
        }
    }
    ways_of_building_substrings[pattern.len()]
}

fn problem(patterns: &Vec<String>, towels: &HashSet<String>, part2: bool) -> usize {
    if part2 {
        return patterns
            .into_iter()
            .map(|pattern| can_build_pattern(pattern.to_string(), towels))
            .sum();
    }
    patterns
        .into_iter()
        .filter(|pattern| can_build_pattern(pattern.to_string(), towels) > 0)
        .count()
}

fn main() {
    let input = read_line_by_line("../../day_19/input/input.txt", '\n');
    let (patterns, towels) = parse_input(&input);

    let problem1 = problem(&patterns, &towels, false);
    println!("{}", problem1);

    let problem2 = problem(&patterns, &towels, true);
    println!("{}", problem2);
}
