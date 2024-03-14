use indexmap::IndexMap;
use std::fs::read_to_string;

fn read_line_by_line(path: &str, separator: char) -> Vec<String> {
	read_to_string(path).unwrap()
	                    .split(separator)
	                    .map(|x| x.trim())
	                    .map(str::to_string)
	                    .collect()
}

fn hash(str: &String) -> u32 {
	let mut hash = 0;
	str.chars().for_each(|c| {
		hash += c as u32;
		hash *= 17;
		hash %= 256;
	});
	return hash;
}

fn first_part() {
	let iterator = read_line_by_line("./input/input.txt", ',');
	let result = iterator.iter()
	                     .map(|x| hash(&x))
	                     .sum::<u32>();
	println!("{}", result);
}

fn second_part() {
	let iterator = read_line_by_line("./input/input.txt", ',');
	let mut boxes: Vec<IndexMap<String, usize>> = vec![];
	for _i in 0..256 { boxes.push(IndexMap::new()); }
	let mut score = 0;

	iterator.iter().for_each(|i| {
		let label: String = if i.contains('-') {
			i.split('-').into_iter().nth(0).unwrap().to_string()
		} else {
			i.split('=').into_iter().nth(0).unwrap().to_string()
		};
		let curr_box = hash(&label) as usize;
		let value = if i.contains('=') { i.split('=').into_iter().nth(1).unwrap() } else { "-" };
		let val_usize = if value != "-" { value.parse().unwrap() } else { 0 };

		if i.contains('-') {
			boxes[curr_box].retain(|x, _y| x != &*label);
		} else {
			boxes[curr_box].insert(label, val_usize);
		}
	});

	boxes.iter().enumerate().for_each(|(x, y)| {
		y.iter().enumerate().for_each(|(a, (_b, c))| {
			score += (x + 1) * (a + 1) * c;
		})
	});
	println!("{}", score);
}

fn main() {
	first_part();
	second_part();
}
