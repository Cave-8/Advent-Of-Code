use std::fs::read_to_string;
use itertools::{Itertools};
use num::integer::lcm;

pub fn read_line_by_line(path: &str, separator: char) -> Vec<String> {
	read_to_string(path).unwrap().split(separator).map(|x| x.trim()).map(str::to_string).collect()
}

fn coordinates_lookup(coordinate: String, coordinates: &Vec<(String, String, String)>) -> (String, String, String) {
	for c in coordinates { if c.0 == coordinate { return c.clone(); } }
	return ("ERR".to_string(), "ERR".to_string(), "ERR".to_string());
}

fn traversal_first_part(coordinates: &Vec<(String, String, String)>, operations: &String) -> i64 {
	let mut steps = 0;
	let mut curr_coord = coordinates_lookup("AAA".to_string(), &coordinates);
	let mut curr_op: usize = 0;
	loop {
		if curr_op == operations.len() { curr_op = 0 } else {
			if curr_coord.0 == "ZZZ" { return steps; } else {
				if operations.chars().nth(curr_op).unwrap() == 'L' {
					curr_coord = coordinates_lookup(curr_coord.1, &coordinates);
				} else {
					curr_coord = coordinates_lookup(curr_coord.2, &coordinates);
				}
				steps += 1;
				curr_op += 1;
			}
		}
	}
}

fn is_ended(curr_coord: &Vec<(String, String, String)>, ghosts: &mut Vec<(i64, bool)>) -> bool {
	let mut index = 0;
	for c in curr_coord {
		if c.0.chars().nth(2).unwrap() == 'Z' {
			if !ghosts[index].1 {
				ghosts[index].0 += 1;
				ghosts[index].1 = true;
			}
		} else if !ghosts[index].1 {
			ghosts[index].0 += 1;
		}
		index += 1;
	}

	return ghosts.into_iter().map(|g| g.1).filter(|g| g == &false).count() == 0;
}

fn traversal_second_part(coordinates: &Vec<(String, String, String)>, operations: &String, ghosts: &mut Vec<(i64, bool)>) -> i64 {
	let mut steps = 0;
	let mut curr_op: usize = 0;

	let mut curr_coord: Vec<(String, String, String)> = Default::default();
	for c in coordinates { if c.0.chars().nth(2).unwrap() == 'A' { curr_coord.push(c.clone()); } }

	loop {
		let mut next_coord: Vec<(String, String, String)> = Default::default();
		if curr_op == operations.len() { curr_op = 0 }

		if is_ended(&curr_coord, ghosts) {
			return ghosts.iter().map(|x| (x.0 - 1, x.1)).map(|(x, y)| x).reduce(|x, y| lcm(x, y)).unwrap();
		}
		for c in curr_coord {
			if operations.chars().nth(curr_op).unwrap() == 'L' {
				next_coord.push(coordinates_lookup(c.1, &coordinates));
			} else {
				next_coord.push(coordinates_lookup(c.2, &coordinates))
			}
		}
		steps += 1;
		curr_op += 1;
		curr_coord = next_coord;
	}
}

fn main() {
	let mut iterator: Vec<String> = read_line_by_line("./input/input.txt", '\n')
		.into_iter()
		.filter(|x| x.len() > 0)
		.collect();
	let operations = iterator.remove(0);
	let coordinates: Vec<(String, String, String)> = iterator.iter()
	                                                         .map(|x| {
		                                                         x.replace(" = (", " ")
		                                                          .replace(",", "")
		                                                          .replace(")", "")
		                                                          .split(" ")
		                                                          .map(str::to_string)
		                                                          .collect::<Vec<String>>()
		                                                          .into_iter()
		                                                          .collect_tuple()
		                                                          .unwrap()
	                                                         })
	                                                         .collect();

	println!("{}", traversal_first_part(&coordinates, &operations));
	let mut ghosts: Vec<(i64, bool)> = vec![(0, false), (0, false), (0, false), (0, false), (0, false), (0, false)];
	println!("{}", traversal_second_part(&coordinates, &operations, &mut ghosts));
}