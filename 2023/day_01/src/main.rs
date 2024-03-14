use std::fs::read_to_string;

pub fn read_line_by_line(path: &str, separator: char) -> Vec<String> {
	read_to_string(path).unwrap().split(separator).map(|x| x.trim()).map(str::to_string).collect()
}

fn calibrate(line: &String) -> u32 {
	let to_numbers = line.chars().into_iter()
	                     .filter(|c| c.is_numeric())
	                     .map(|c| c.to_digit(10).unwrap())
	                     .collect::<Vec<u32>>();
	let first_digit = to_numbers.first().unwrap();
	let second_digit = to_numbers.last().unwrap();
	return first_digit * 10 + second_digit;
}

fn words_to_number(line: &mut String) -> String {
	line.replace("one", "o1e")
		.replace("two", "t2o")
		.replace("three", "t3e")
		.replace("four", "f4r")
		.replace("five", "f5e")
		.replace("six", "s6x")
		.replace("seven", "s7n")
		.replace("eight", "e8t")
		.replace("nine", "n9e")
}

fn problem(variant: i32) {
	let mut iterator: Vec<String> = read_line_by_line("./input/input.txt", '\n');
	if variant == 2 { iterator = iterator.iter_mut().map(|s| words_to_number(s)).collect::<Vec<String>>(); }
	let result: u32 = iterator.iter().map(|s| calibrate(&s)).collect::<Vec<u32>>().iter().sum();
	println!("{}", result);
}

fn main() {
	problem(1);
	problem(2);
}

