use std::clone::Clone;
use std::collections::{HashMap};
use std::fs::read_to_string;
use std::iter::Iterator;
use std::string::ToString;
use std::sync::Mutex;
use lazy_static::lazy_static;

// Global cache
lazy_static! {
    static ref CACHE: Mutex<HashMap<(String, Vec<usize>), i64>> = Mutex::new(HashMap::new());
}

fn read_line_by_line(path: &str, separator: char) -> Vec<String> {
    read_to_string(path).unwrap().split(separator).map(|x| x.trim()).map(str::to_string).collect()
}

fn count_blocks_memoized(str: &String, config: &Vec<usize>) -> i64 {
    // Already in cache
    let curr_el = (str.clone(), config.clone());
    if CACHE.lock().unwrap().contains_key(&curr_el) { return *CACHE.lock().unwrap().get(&curr_el).unwrap(); }

    // Base cases
    // Ended string with all groups found
    if str.len() == 0 && config.len() == 0 {
        return 1;
    }
    // Ended string but some groups weren't found
    else if str.len() == 0 && config.len() != 0 {
        return 0;
    }
    // All group found but some broken spring are still in the string
    if config.len() == 0 && str.contains("#") {
        return 0;
    }
    // All group found, some other chars to read but no broken spring inside it
    else if config.len() == 0 && !str.contains("#") {
        return 1;
    }
    let mut final_val = 0;

    // Skip char and ignore
    let curr_char = str.chars().nth(0).unwrap();
    if curr_char == '.' || curr_char == '?' {
        final_val += count_blocks_memoized(&(str.chars().skip(1).collect::<String>()), &config);
    }
    if curr_char == '#' || curr_char == '?' {
        let curr_config = config.get(0).unwrap();
        // Enough groups for remaining chars
        let cond1 = curr_config <= &str.len();
        // No working spring in first curr_config chars
        let cond2 = str.chars().take(*curr_config).filter(|x| *x == '.').count() == 0;
        if  cond1 && cond2 && (*curr_config == str.len() || str.chars().nth(*curr_config).unwrap() != '#')
        {
            let substr = str.clone().chars().skip(config.get(0).unwrap() + 1).collect::<String>();
            let mut sub_conf = config.clone();
            sub_conf.remove(0);
            final_val += count_blocks_memoized(&substr, &sub_conf);
        }
    }
    // Add value to cache for later
    CACHE.lock().unwrap().insert((str.clone(), config.clone()), final_val);
    return final_val;
}

fn problem(variant: i32) {
    let iterator = read_line_by_line("./input/input.txt", '\n');
    let mut spring_config: Vec<(String, Vec<usize>)> = vec![];
    let mut sum: i64 = 0;
    iterator.iter().for_each(|s| {
        let s_c: Vec<_> = s.split(' ').collect();
        let mut spring = s_c[0].to_string();
        let mut config = s_c[1].split(',').map(|x| x.parse::<usize>().unwrap()).collect::<Vec<usize>>();

        if variant == 2 {
            spring.push('?');
            spring.push_str(spring.repeat(3).as_str());
            spring.push_str(s_c[0].to_string().as_str());
            config = config.repeat(5);
        }

        spring_config.push((spring, config));
    });

    spring_config.iter().for_each(|(x, y)| {
        sum += count_blocks_memoized(&x, &y);
    });
    println!("{}", sum);
}

fn main() {
    problem(1);
    problem(2);
}