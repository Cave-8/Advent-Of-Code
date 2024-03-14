use std::fs::read_to_string;
use itertools::Itertools;

pub fn read_line_by_line(path: &str, separator: char) -> Vec<String> {
	read_to_string(path).unwrap().split(separator).map(|x| x.trim()).map(str::to_string).collect()
}

fn pair_sum(t0: (i64, i64), t1: (i64, i64)) -> (i64, i64) {
	(t0.0 + t1.0, t0.1 + t1.1)
}

// Like day 10 with Pick's theorem
fn compute_blocks(info: &mut Vec<(char, i64)>) -> i64 {
	let directions = vec![(0, 1), (1, 0), (0, -1), (-1, 0)];
	let mut points = vec![(0, 0)];
	while !info.is_empty() {
		let op = info.remove(0);
		for _i in 0..op.1 {
			match op.0 {
				'R' => points.push(pair_sum(points.last().unwrap().clone(), (1 * directions[0].0, 1 * directions[0].1))),
				'D' => points.push(pair_sum(points.last().unwrap().clone(), (1 * directions[1].0, 1 * directions[1].1))),
				'L' => points.push(pair_sum(points.last().unwrap().clone(), (1 * directions[2].0, 1 * directions[2].1))),
				'U' => points.push(pair_sum(points.last().unwrap().clone(), (1 * directions[3].0, 1 * directions[3].1))),
				_ => ()
			}
		}
	}
	points.pop();

	let perimeter: i64 = points.len() as i64;
	let mut area = points.iter().circular_tuple_windows().map(|(x, y)| x.0 * y.1 - x.1 * y.0).sum::<i64>() / 2;
	if area < 0 { area = -area }
	return perimeter + (area - perimeter / 2 + 1);
}

fn problem(variant: usize) {
	let iterator = read_line_by_line("./input/input.txt", '\n');
	let mut info: Vec<(char, i64)> = vec![];
	let mut encoded_info: Vec<String> = vec![];
	if variant == 2 {
		iterator.iter().for_each(|x| {
			let mut components: Vec<&str> = x.split(" ").collect();
			encoded_info.push(components[2].to_string());
		});

		for m in encoded_info {
			let mut hex_coord = m.chars().skip(2).take(5).collect::<String>();
			let hex_dir = m.chars().skip(7).take(1).collect::<String>().pop().unwrap();

			let blocks = i64::from_str_radix(&*hex_coord, 16).unwrap();
			let dir = match hex_dir {
				'0' => 'R',
				'1' => 'D',
				'2' => 'L',
				'3' => 'U',
				_ => '\\',
			};
			info.push((dir, blocks));
		}
	} else {
		iterator.iter().for_each(|x| {
			let mut components: Vec<&str> = x.split(" ").collect();
			info.push((components[0].parse().unwrap(), components[1].parse().unwrap()));
		});
	}
	println!("{:?}", compute_blocks(&mut info));
}

fn main() {
	problem(1);
	problem(2);
}