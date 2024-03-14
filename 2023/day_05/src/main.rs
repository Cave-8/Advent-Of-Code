use std::fs::read_to_string;
use crate::MapTypes::{DEFAULT, FTW, HTL, LTT, STF, STS, TTH, WTL};

enum MapTypes {
    DEFAULT,
    STS,
    STF,
    FTW,
    WTL,
    LTT,
    TTH,
    HTL,
}

fn input_reader(path: String) -> String {
    read_to_string(path).unwrap()
}

fn number_finder(curr_num: i64, map: &Vec<(i64, i64, i64)>, variant: usize) -> i64 {
    match variant {
        1 => {
            for i in map {
                if curr_num >= i.1 && curr_num <= i.1 + (i.2 - 1) {
                    return i.0 + (curr_num - i.1);
                }
            }
            curr_num
        }
        2 => {
            for i in map {
                if curr_num >= i.0 && curr_num <= i.0 + (i.2 - 1) {
                    return i.1 + (curr_num - i.0);
                }
            }
            curr_num
        }
        _ => curr_num
    }
}

fn problem(second_part: bool) {
    let mut index: MapTypes = DEFAULT;
    let mut iterator: Vec<String> = input_reader("./input/input.txt".to_string()).split("\n")
                                                                                 .filter(|s| s.len() > 1)
                                                                                 .map(str::to_string)
                                                                                 .collect();

    let mut seeds: Vec<i64> = iterator[0].split(": ")
                                         .nth(1)
                                         .unwrap()
                                         .trim()
                                         .split(" ")
                                         .map(|x| x.parse::<i64>()
                                                   .unwrap())
                                         .collect();
    iterator.remove(0);

    let mut sts_vec: Vec<(i64, i64, i64)> = Default::default();
    let mut stf_vec: Vec<(i64, i64, i64)> = Default::default();
    let mut ftw_vec: Vec<(i64, i64, i64)> = Default::default();
    let mut wtl_vec: Vec<(i64, i64, i64)> = Default::default();
    let mut ltt_vec: Vec<(i64, i64, i64)> = Default::default();
    let mut tth_vec: Vec<(i64, i64, i64)> = Default::default();
    let mut htl_vec: Vec<(i64, i64, i64)> = Default::default();

    iterator.iter().for_each(|s| {
        let curr_row = s.as_str().trim();
        match curr_row {
            "seed-to-soil map:" => index = STS,
            "soil-to-fertilizer map:" => index = STF,
            "fertilizer-to-water map:" => index = FTW,
            "water-to-light map:" => index = WTL,
            "light-to-temperature map:" => index = LTT,
            "temperature-to-humidity map:" => index = TTH,
            "humidity-to-location map:" => index = HTL,
            _ => {
                let tmp: Vec<i64> = curr_row.split(" ").map(|x| x.parse::<i64>().unwrap()).collect();
                let new_tup: (i64, i64, i64) = (tmp[0], tmp[1], tmp[2]);
                match index {
                    STS => sts_vec.push(new_tup),
                    STF => stf_vec.push(new_tup),
                    FTW => ftw_vec.push(new_tup),
                    WTL => wtl_vec.push(new_tup),
                    LTT => ltt_vec.push(new_tup),
                    TTH => tth_vec.push(new_tup),
                    HTL => htl_vec.push(new_tup),
                    _ => {}
                }
            }
        }
    });

    if second_part {
        let mut new_seeds: Vec<(i64, i64)> = Default::default();
        let mut i = 0;
        loop {
            new_seeds.push((seeds[i], seeds[i] + seeds[i + 1] - 1));
            i += 2;
            if i >= seeds.len() { break; }
        }


        new_seeds.sort();
        let mut i: i64 = 0;
        loop {
            let mut curr_num = i;
            curr_num = number_finder(curr_num, &htl_vec, 2);
            curr_num = number_finder(curr_num, &tth_vec, 2);
            curr_num = number_finder(curr_num, &ltt_vec, 2);
            curr_num = number_finder(curr_num, &wtl_vec, 2);
            curr_num = number_finder(curr_num, &ftw_vec, 2);
            curr_num = number_finder(curr_num, &stf_vec, 2);
            curr_num = number_finder(curr_num, &sts_vec, 2);

            for s in &new_seeds {
                if curr_num >= s.0 && curr_num <= s.1 {
                    println!("{}", i);
                    return;
                }
            }
            i += 1;
        }
    }

    let minimum = seeds.iter().map(|x| {
        let mut curr_num: i64 = *x;
        curr_num = number_finder(curr_num, &sts_vec, 1);
        curr_num = number_finder(curr_num, &stf_vec, 1);
        curr_num = number_finder(curr_num, &ftw_vec, 1);
        curr_num = number_finder(curr_num, &wtl_vec, 1);
        curr_num = number_finder(curr_num, &ltt_vec, 1);
        curr_num = number_finder(curr_num, &tth_vec, 1);
        curr_num = number_finder(curr_num, &htl_vec, 1);
        curr_num
    }).min();

    println!("{}", minimum.unwrap());
}

fn main() {
    problem(false);
    problem(true);
}