use std::any::Any;
use std::fs::read_to_string;

fn input_reader(path: String) -> String { read_to_string(path).unwrap() }

fn quadratic_distance(total_time: i64, record: i64) -> i64 {
    let delta_sqrt = f64::sqrt(total_time as f64 * total_time as f64 - (4 * record) as f64);
    let max = f64::floor((-total_time as f64 + delta_sqrt) / 2 as f64);
    let min = f64::ceil((-total_time as f64 - delta_sqrt) / 2 as f64);

    (max - min + 1.0) as i64
}

fn first_part(iter: &Vec<String>) {
    let iterator = iter.clone();
    let times: Vec<i64> = iterator[0].trim()
                                     .split(":")
                                     .nth(1).unwrap()
                                     .split(" ")
                                     .map(|x| x.trim())
                                     .filter(|x| x != &"")
                                     .map(|x| x.parse::<i64>().unwrap())
                                     .collect();
    let records: Vec<i64> = iterator[1].trim()
                                       .split(":").nth(1).unwrap()
                                       .split(" ")
                                       .map(|x| x.trim())
                                       .filter(|x| x != &"")
                                       .map(|x| x.parse::<i64>().unwrap())
                                       .collect();
    let indexer: Vec<usize> = (0..times.len()).collect();
    let result: i64 = indexer.into_iter()
                             .map(|x| quadratic_distance(times[x], records[x]))
                             .reduce(|x, y| x * y)
                             .unwrap();

    println!("{}", result);
}

fn second_part(iter: &Vec<String>) {
    let iterator = iter.clone();
    let times: i64 = iterator[0].trim()
                                .split(":")
                                .nth(1).unwrap()
                                .replace(" ", "")
                                .parse::<i64>()
                                .unwrap();
    let record = iterator[1].trim()
                            .split(":")
                            .nth(1).unwrap()
                            .replace(" ", "")
                            .parse::<i64>()
                            .unwrap();

    println!("{}", quadratic_distance(times, record));
}

fn main() {
    let iterator: Vec<String> = input_reader("./input/input.txt".to_string())
        .split("\n")
        .map(str::to_string)
        .filter(|x| x.len() > 0)
        .collect();

    first_part(&iterator);
    second_part(&iterator);
}