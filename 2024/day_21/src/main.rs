use itertools::Itertools;
use lazy_static::lazy_static;
use std::collections::HashMap;
use std::sync::Mutex;
use utils::read_line_by_line;

lazy_static! {
    static ref SEQ_CACHE: Mutex<HashMap<(char, char), Vec<String>>> = Mutex::new(HashMap::new());
}
lazy_static! {
    static ref COST_CACHE: Mutex<HashMap<(char, char, usize), usize>> = Mutex::new(HashMap::new());
}
/////////////
// CACHING //
/////////////
fn gen_sequences(characters: &[char], sequence_length: usize) -> Vec<String> {
    let chars_repeated = characters.repeat(sequence_length);
    let sequences: Vec<String> = chars_repeated
        .iter()
        .permutations(sequence_length)
        .unique()
        .map(|c| c.iter().join(""))
        .collect();
    sequences
}

fn get_sequences(start: char, end: char, keypad: &HashMap<char, (i32, i32)>) -> Vec<String> {
    let directions = HashMap::from([('<', (0, -1)), ('^', (-1, 0)), ('>', (0, 1)), ('v', (1, 0))]);
    let (starting_coords, ending_coords) =
        (*keypad.get(&start).unwrap(), *keypad.get(&end).unwrap());
    let (dx, dy) = (
        ending_coords.0 - starting_coords.0,
        ending_coords.1 - starting_coords.1,
    );
    // It's the sequence length
    let manhattan_distance = (dx.abs() + dy.abs()) as usize;
    let mut moves: Vec<char> = Vec::new();
    if dx > 0 {
        moves.push('v');
    } else if dx < 0 {
        moves.push('^');
    }
    if dy > 0 {
        moves.push('>');
    } else if dy < 0 {
        moves.push('<');
    }
    let sequences = gen_sequences(&moves, manhattan_distance);

    sequences
        .into_iter()
        .filter(|seq| {
            let (mut cx, mut cy) = starting_coords;
            for c in seq.chars() {
                cx += directions[&c].0;
                cy += directions[&c].1;
                if keypad.len() > 6 {
                    if (cx, cy) == (3, 0) || cx < 0 || cx > 3 || cy < 0 || cy > 2 {
                        return false;
                    }
                } else {
                    if (cx, cy) == (0, 0) || cx < 0 || cx > 1 || cy < 0 || cy > 2 {
                        return false;
                    }
                }
            }
            return (cx, cy) == ending_coords;
        })
        .map(|mut str| {
            str.push('A');
            str
        })
        .collect()
}

// Possible moves can be precomputed at compile time
fn build_cache() {
    let numerical_keypad: HashMap<char, (i32, i32)> = HashMap::from([
        ('0', (3, 1)),
        ('A', (3, 2)),
        ('1', (2, 0)),
        ('2', (2, 1)),
        ('3', (2, 2)),
        ('4', (1, 0)),
        ('5', (1, 1)),
        ('6', (1, 2)),
        ('7', (0, 0)),
        ('8', (0, 1)),
        ('9', (0, 2)),
    ]);
    let directional_keypad: HashMap<char, (i32, i32)> = HashMap::from([
        ('>', (1, 2)),
        ('v', (1, 1)),
        ('<', (1, 0)),
        ('^', (0, 1)),
        ('A', (0, 2)),
    ]);

    // Precompute LUTs
    let nums = Vec::from(['0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'A']);
    let dirs = Vec::from(['<', '>', 'v', '^', 'A']);
    let nums_pairs: Vec<(char, char)> = nums
        .iter()
        .cartesian_product(nums.iter())
        .map(|(a, b)| (*a, *b))
        .collect();
    let dirs_pairs: Vec<(char, char)> = dirs
        .iter()
        .cartesian_product(dirs.iter())
        .map(|(a, b)| (*a, *b))
        .collect();
    let sequences = nums_pairs
        .into_iter()
        .map(|(s, e)| ((s, e), get_sequences(s, e, &numerical_keypad)))
        .collect::<Vec<_>>();
    sequences.into_iter().for_each(|(k, v)| {
        let _ = SEQ_CACHE.lock().unwrap().insert(k, v);
    });
    let sequences_dirs = dirs_pairs
        .into_iter()
        .map(|(s, e)| ((s, e), get_sequences(s, e, &directional_keypad)))
        .collect::<Vec<_>>();
    sequences_dirs.into_iter().for_each(|(k, v)| {
        let _ = SEQ_CACHE.lock().unwrap().insert(k, v);
    });
}

// Clean cache for second part
fn clean_cache() {
    let mut lock = SEQ_CACHE.lock().unwrap();
    lock.drain();
    let mut lock = COST_CACHE.lock().unwrap();
    lock.drain();
}

////////////////////
// ACTUAL PROBLEM //
////////////////////
fn compute_local_cost(a: char, b: char, curr_depth: usize, depth: usize) -> usize {
    // Check cache
    let lock = COST_CACHE.lock().unwrap();
    match lock.get(&(a, b, curr_depth)) {
        None => drop(lock),
        Some(cached_result) => return *cached_result,
    }

    if curr_depth == depth {
        let lock = SEQ_CACHE.lock().unwrap();
        return lock.get(&(a, b)).unwrap().get(0).unwrap().len();
    }

    let lock = SEQ_CACHE.lock().unwrap();
    let sequences_to_try = lock.get(&(a, b)).unwrap().clone();
    let complete_sequences: Vec<String> = sequences_to_try
        .iter()
        .map(|seq| {
            let mut str = seq.clone();
            str.insert_str(0, "A");
            str
        })
        .collect();
    drop(lock);

    let res = complete_sequences
        .into_iter()
        .map(|seq| {
            seq.chars()
                .tuple_windows()
                .into_iter()
                .map(|(a, b)| compute_local_cost(a, b, curr_depth + 1, depth))
                .sum()
        })
        .min()
        .unwrap();

    // Update cache
    let mut lock = COST_CACHE.lock().unwrap();
    lock.insert((a, b, curr_depth), res);
    res
}

fn compute_cost(code: &str, robots: usize) -> usize {
    code.chars()
        .tuple_windows()
        .into_iter()
        .map(|(a, b)| {
            let lock = COST_CACHE.lock().unwrap();
            match lock.get(&(a, b, robots)) {
                None => {
                    drop(lock);
                    compute_local_cost(a, b, 0, robots)
                }
                Some(cached_result) => return *cached_result,
            }
        })
        .sum()
}

fn problem(input: &Vec<String>, robots: usize) -> usize {
    input
        .iter()
        .map(|code| {
            // We start from A
            let mut complete_code = code.clone();
            complete_code.insert_str(0, "A");
            compute_cost(complete_code.as_str(), robots)
                * code
                    .chars()
                    .take_while(|c| c.is_digit(10))
                    .join("")
                    .parse::<usize>()
                    .unwrap()
        })
        .sum()
}

fn main() {
    let input = read_line_by_line("../../day_21/input/input.txt", '\n');

    build_cache();
    let part1 = problem(&input, 2);
    println!("{}", part1);

    clean_cache();
    build_cache();
    let part2 = problem(&input, 25);
    println!("{}", part2);
}