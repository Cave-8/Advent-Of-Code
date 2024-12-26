use itertools::Itertools;
use utils::read_line_by_line;

type LK = (i32, i32, i32, i32, i32);
fn parse_input(input: &[String]) -> (Vec<LK>, Vec<LK>) {
    let mut locks: Vec<LK> = Vec::new();
    let mut keys: Vec<LK> = Vec::new();
    let mut is_lock = true;
    let mut temp_lock = (0, 0, 0, 0, 0);
    let mut temp_key = (6, 6, 6, 6, 6);
    for (index, line) in input.iter().enumerate() {
        if line.len() < 2 {
            if is_lock {
                locks.push(temp_lock);
            } else {
                keys.push(temp_key);
            }
            temp_lock = (0, 0, 0, 0, 0);
            temp_key = (6, 6, 6, 6, 6);
            is_lock = true;
        }
        if index % 8 == 0 {
            if line.contains(".") {
                is_lock = false;
            }
        }
        if is_lock {
            line.chars().enumerate().for_each(|(i, c)| {
                if c == '#' && index % 8 != 0 {
                    match i {
                        0 => temp_lock.0 += 1,
                        1 => temp_lock.1 += 1,
                        2 => temp_lock.2 += 1,
                        3 => temp_lock.3 += 1,
                        4 => temp_lock.4 += 1,
                        _ => (),
                    }
                }
            });
        } else {
            line.chars().enumerate().for_each(|(i, c)| {
                if c == '.' {
                    match i {
                        0 => temp_key.0 -= 1,
                        1 => temp_key.1 -= 1,
                        2 => temp_key.2 -= 1,
                        3 => temp_key.3 -= 1,
                        4 => temp_key.4 -= 1,
                        _ => (),
                    }
                }
            });
        }
    }
    (locks, keys)
}
fn lk_sum(lock: &LK, key: &LK) -> LK {
    (
        lock.0 + key.0,
        lock.1 + key.1,
        lock.2 + key.2,
        lock.3 + key.3,
        lock.4 + key.4,
    )
}
fn lk_compare(op1: &LK, op2: &LK) -> bool {
    op1.0 <= op2.0 && op1.1 <= op2.1 && op1.2 <= op2.2 && op1.3 <= op2.3 && op1.4 <= op2.4
}
fn problem(locks: &[LK], keys: &[LK]) -> usize {
    locks
        .iter()
        .cartesian_product(keys)
        .into_iter()
        .filter(|(lock, key)| lk_compare(&lk_sum(lock, key), &(5, 5, 5, 5, 5)))
        .collect::<Vec<(&LK, &LK)>>()
        .len()
}

fn main() {
    let input = read_line_by_line("../../day_25/input/input.txt", '\n');
    let (locks, keys) = parse_input(&input);

    let problem = problem(&locks, &keys);
    println!("{}", problem);
}
