use edit_distance::edit_distance;
use std::cmp::min;
use std::fs::read_to_string;

fn reflection_first_part(iter: &Vec<String>) -> usize {
    for i in 0..iter.len() - 1 {
        let mut valid = false;
        let curr = iter.get(i).unwrap();
        let next = iter.get(i + 1).unwrap();
        if curr == next {
            valid = true;

            for j in 1..=min(i, iter.len() - i - 2) {
                let prev = iter.get(i - j).unwrap();
                let succ = iter.get(i + j + 1).unwrap();
                if prev != succ {
                    valid = false;
                    break;
                }
            }
        }
        if valid {
            return i + 1;
        }
    }
    return 0;
}

fn reflection_second_part(iter: &Vec<String>) -> usize {
    for i in 0..iter.len() - 1 {
        let mut errors = 1;
        let mut valid = false;
        let curr = iter.get(i).unwrap();
        let next = iter.get(i + 1).unwrap();

        if curr == next || edit_distance(curr, next) == 1 {
            if edit_distance(curr, next) == 1 {
                errors -= 1;
            }
            valid = true;

            for j in 1..=min(i, iter.len() - i - 2) {
                let prev = iter.get(i - j).unwrap();
                let succ = iter.get(i + j + 1).unwrap();
                if prev != succ {
                    if errors == 1 && edit_distance(prev, succ) == 1 {
                        errors -= 1;
                    } else {
                        valid = false;
                        break;
                    }
                }
            }
        }
        if valid && errors == 0 {
            return i + 1;
        };
    }
    return 0;
}

fn main() {
    let patterns = read_to_string("./input/input.txt")
        .unwrap()
        .split("\r\n\r\n")
        .map(str::to_string)
        .map(|x| x.replace("\r\n", " "))
        .collect::<Vec<String>>();

    let mut result_p1 = 0;
    let mut result_p2 = 0;
    patterns.iter().for_each(|p| {
        let rows = p
            .split(" ")
            .map(|x| x.trim())
            .map(str::to_string)
            .collect::<Vec<String>>();
        let cols = (0..rows[0].len())
            .map(|c| {
                rows.iter()
                    .map(|r| r.chars().nth(c).unwrap())
                    .collect::<String>()
            })
            .collect::<Vec<String>>();

        result_p1 += reflection_first_part(&cols);
        result_p1 += 100 * reflection_first_part(&rows);
        result_p2 += reflection_second_part(&cols);
        result_p2 += 100 * reflection_second_part(&rows);
    });
    println!("{}", result_p1);
    println!("{}", result_p2);
}
