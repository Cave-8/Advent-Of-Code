use std::collections::HashMap;
use utils::*;

fn create_rules(rules: Vec<String>) -> HashMap<i32, Vec<i32>> {
    let mut map: HashMap<i32, Vec<i32>> = HashMap::new();
    for rule in rules {
        let elements: Vec<&str> = rule.split("|").collect::<Vec<_>>();
        let element_before = elements[1].parse::<i32>().unwrap();
        let element_after = elements[0].parse::<i32>().unwrap();

        map.entry(element_before)
            .or_insert(Vec::new())
            .push(element_after);
    }
    map
}

fn create_updates(updates: Vec<String>) -> Vec<Vec<i32>> {
    updates
        .iter()
        .map(|x| {
            x.split(",")
                .map(|x| x.parse::<i32>().unwrap())
                .collect::<Vec<_>>()
        })
        .collect::<Vec<_>>()
}

fn check_rules(rules: &HashMap<i32, Vec<i32>>, update: &Vec<i32>) -> bool {
    let mut flag = true;
    for (i, num_i) in update.iter().enumerate() {
        let next_numbers_in_update: Vec<i32> = update
            .iter()
            .enumerate()
            .filter(|(j, _)| j > &i)
            .map(|(_, el)| *el)
            .collect::<Vec<i32>>();

        let pages_before = match rules.get(&num_i) {
            Some(x) => x,
            None => continue,
        };

        for num_j in next_numbers_in_update {
            if pages_before.contains(&num_j) {
                flag = false;
                break;
            }
        }
    }
    flag
}

fn rearrange(rules: &HashMap<i32, Vec<i32>>, update: &Vec<i32>) -> Vec<i32> {
    let mut final_update = update.clone();
    let mut swap_i = 0;
    let mut swap_j = 0;
    let mut restart = false;
    loop {
        swap_i = 0;
        swap_j = 0;
        restart = false;
        for (i, num_i) in final_update.iter().enumerate() {
            let next_numbers_in_update: Vec<i32> = final_update
                .iter()
                .enumerate()
                .filter(|(j, _)| j > &i)
                .map(|(_, el)| *el)
                .collect::<Vec<i32>>();

            let pages_before = match rules.get(&num_i) {
                Some(x) => x,
                None => continue,
            };

            for (j, num_j) in next_numbers_in_update.iter().enumerate() {
                if pages_before.contains(&num_j) {
                    swap_i = i;
                    swap_j = i + j + 1;
                    restart = true;
                    break;
                }
            }
        }
        if swap_i != swap_j {
            final_update.swap(swap_i, swap_j);
        }
        if !restart {
            break;
        }
    }
    final_update
}

fn problem1(rules: &HashMap<i32, Vec<i32>>, updates: &Vec<Vec<i32>>) -> i32 {
    updates
        .iter()
        .filter(|update| check_rules(&rules, &update))
        .map(|update| update[update.len() / 2])
        .sum()
}

fn problem2(rules: &HashMap<i32, Vec<i32>>, updates: &Vec<Vec<i32>>) -> i32 {
    updates
        .iter()
        .filter(|update| !check_rules(&rules, &update))
        .map(|update_to_correct| rearrange(&rules, &update_to_correct)[update_to_correct.len() / 2])
        .sum()
}

fn main() {
    let input = read_line_by_line("../../day_05/input/input.txt", '\n');
    let parts = input.split(|x| x == "").collect::<Vec<_>>();
    let (first_sec, second_sec) = (parts[0], parts[1]);

    let rules: HashMap<i32, Vec<i32>> = create_rules(Vec::from(first_sec));
    let updates = create_updates(Vec::from(second_sec));

    let problem1 = problem1(&rules, &updates);
    println!("{}", problem1);
    let problem2 = problem2(&rules, &updates);
    println!("{}", problem2);
}
