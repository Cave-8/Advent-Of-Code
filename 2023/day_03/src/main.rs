use std::fs::read_to_string;

pub fn read_line_by_line(path: &str, separator: char) -> Vec<String> {
    read_to_string(path).unwrap().split(separator).map(|x| x.trim()).map(str::to_string).collect()
}

fn string_to_numeric(num: &String) -> u32 {
    if num == "" {
        return 0;
    }
    num.parse().unwrap()
}

fn is_symbol(c: char) -> bool {
    if !c.is_digit(10) && c != '.' && c != '\n' { true } else { false }
}

fn search_gears_numbers(iter: &Vec<String>, row_num: usize, col_num: usize) -> u32 {
    let curr_row = row_num as isize;
    let curr_col = col_num as isize;
    let mut first_number = 0;
    let mut second_number = 0;
    let mut known_coords: Vec<(usize, usize)> = Default::default();
    let directions: Vec<(isize, isize)> = vec![(-1, -1), (-1, 0), (-1, 1), (0, -1), (0, 1), (1, -1), (1, 0), (1, 1)];

    for (nr, nc) in directions {
        let r = (curr_row + nr) as usize;
        let c = (curr_col + nc) as usize;
        if iter.iter().nth(r).unwrap().chars().nth(c).unwrap().is_digit(10) {
            if first_number == 0 {
                first_number = find_numbers(&iter, r, c, &mut known_coords);
            } else if second_number == 0 {
                second_number = find_numbers(&iter, r, c, &mut known_coords);
            }
        }
    }
    first_number * second_number
}

fn find_numbers(iter: &Vec<String>, row_num: usize, col_num: usize, coords: &mut Vec<(usize, usize)>) -> u32 {
    let mut string_num: String = "".to_string();
    let mut starting_col = col_num;

    loop {
        if starting_col != 0 && iter.iter().nth(row_num).unwrap().chars().nth(starting_col - 1).unwrap().is_digit(10) {
            starting_col -= 1;
        } else { break; }
    }

    if coords.iter().any(|x| { x.0 == row_num && x.1 == starting_col }) {
        return 0;
    } else {
        coords.push((row_num, starting_col));
    }

    loop {
        if starting_col != iter.iter().len() && iter.iter().nth(row_num).unwrap().chars().nth(starting_col).unwrap().is_digit(10) {
            string_num.push(iter.iter().nth(row_num).unwrap().chars().nth(starting_col).unwrap());
            starting_col += 1;
        } else { break; }
    }
    if string_num != "" { string_num.parse().unwrap() } else { 0 }
}

fn part_numbers(iter: &Vec<String>, row_num: usize, col_num: usize) -> bool {
    let rows_numb = iter.len() - 1;
    let col_len = iter.iter().nth(0).unwrap().len() - 1;
    let directions: Vec<(isize, isize)> = vec![(-1, -1), (-1, 0), (-1, 1), (0, -1), (0, 1), (1, -1), (1, 0), (1, 1)];

    for (nr, nc) in directions {
        let r = (row_num as isize + nr) as usize;
        let c = (col_num as isize + nc) as usize;

        if r < 0 || c < 0 || r > rows_numb || c > col_len { continue }
        if is_symbol(iter.iter().nth(r).unwrap().chars().nth(c).unwrap()) { return true }
    }
    return false
}

fn first_part() {
    let iterator: Vec<String> = read_line_by_line("./input/input.txt", '\n');
    let mut final_number = 0;
    let mut rows_counter = 0;

    for i in &iterator {
        let mut string_num = String::from("");
        let mut part_num = false;

        for j in 0..i.len() {
            if i.chars().nth(j).unwrap().is_digit(10) {
                string_num.push(i.chars().nth(j).unwrap());
                if !part_num {
                    part_num = part_numbers(&iterator, rows_counter, j);
                }
            } else if part_num {
                final_number += string_to_numeric(&string_num);
                string_num = String::from("");
                part_num = false;
            } else {
                string_num = String::from("");
            }
        }

        if part_num {
            final_number += string_to_numeric(&string_num);
            string_num = String::from("");
            part_num = false;
        }
        rows_counter += 1;
    }
    println!("{}", final_number);
}

fn second_part() {
    let iterator: Vec<String> = read_line_by_line("./input/input.txt", '\n');
    let mut final_number = 0;
    let mut rows_counter = 0;

    for i in &iterator {
        for j in 0..i.len() {
            if i.chars().nth(j).unwrap() == '*' {
                final_number += search_gears_numbers(&iterator, rows_counter, j);
            }
        }
        rows_counter += 1;
    }
    println!("{}", final_number);
}

fn main() {
    first_part();
    second_part();
}