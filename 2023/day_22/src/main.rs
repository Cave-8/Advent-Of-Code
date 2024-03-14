use std::cmp::Ordering;
use std::collections::HashMap;
use std::fs::read_to_string;

#[derive(Debug, Eq, PartialEq, Clone)]
struct Brick {
	x0: i32,
	x1: i32,
	y0: i32,
	y1: i32,
	z0: i32,
	z1: i32,
	occupied_spaces: Vec<(i32, i32, i32)>,
}

impl PartialOrd<Self> for Brick {
	fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
		self.z0.partial_cmp(&other.z0)
	}
}

impl Ord for Brick {
	fn cmp(&self, other: &Self) -> Ordering {
		self.z0.partial_cmp(&other.z0).unwrap()
	}
}

impl Brick {
	fn lower_z(&mut self) {
		self.z0 -= 1;
		self.z1 -= 1;
	}
	fn lower_occ_spaces(&mut self) {
		let mut new_spaces = vec![];
		for o in self.occupied_spaces.iter() {
			new_spaces.push((o.0, o.1, o.2 - 1));
		}
		self.occupied_spaces = new_spaces;
	}
}

pub fn read_line_by_line(path: &str, separator: char) -> Vec<String> {
	read_to_string(path).unwrap().split(separator).map(|x| x.trim()).map(str::to_string).collect()
}

fn empty_below(cells: &HashMap<(i32, i32, i32), bool>, brick: &Brick) -> bool {
	let mut ok = true;
	brick.occupied_spaces.iter().for_each(|&o| {
		if !brick.occupied_spaces.contains(&(o.0, o.1, o.2 - 1)) {
			if cells.contains_key(&(o.0, o.1, o.2 - 1)) || o.2 - 1 == 0 { ok = false }
		}
	});
	return ok;
}

fn settle_bricks(bricks: &mut Vec<Brick>) -> i32 {
	let mut cells: HashMap<(i32, i32, i32), bool> = HashMap::new();
	let mut moved_bricks: i32 = 0;

	bricks.sort();
	bricks.iter().for_each(|b| {
		b.occupied_spaces.iter().for_each(|&o| {
			cells.insert(o, true);
		});
	});
	bricks.iter_mut().for_each(|b| {
		let mut counted = false;
		loop {
			if b.z0 - 1 != 0 && empty_below(&cells, &b) {
				if !counted { moved_bricks += 1 };
				counted = true;

				b.occupied_spaces.iter().for_each(|o| {
					cells.remove(o);
				});
				b.lower_occ_spaces();
				b.lower_z();
				b.occupied_spaces.iter().for_each(|&o| {
					cells.insert(o, true);
				});
			} else { break; }
		}
	});

	return moved_bricks;
}

fn problem() {
	let iterator = read_line_by_line("./input/input.txt", '\n');
	let mut bricks: Vec<Brick> = iterator.iter().map(|x|
		{
			let coords = x.split("~")
			              .map(str::to_string)
			              .collect::<Vec<String>>();

			let s = coords[0].split(",")
			                 .map(|x| x.parse::<i32>().unwrap())
			                 .collect::<Vec<i32>>();

			let e = coords[1].split(",")
			                 .map(|x| x.parse::<i32>().unwrap())
			                 .collect::<Vec<i32>>();

			let mut occupied_spaces: Vec<(i32, i32, i32)> = vec![];

			(s[0]..=e[0]).into_iter().enumerate().for_each(|(_, i)| {
				(s[1]..=e[1]).into_iter().enumerate().for_each(|(_, j)| {
					(s[2]..=e[2]).into_iter().enumerate().for_each(|(_, k)| {
						occupied_spaces.push((i, j, k));
					});
				});
			});

			return Brick { x0: s[0], x1: e[0], y0: s[1], y1: e[1], z0: s[2], z1: e[2], occupied_spaces };
		}).collect::<Vec<Brick>>();

	settle_bricks(&mut bricks);
	let moved_bricks =
		(0..bricks.len()).map(|i| {
			let mut curr_bricks = bricks.clone();
			curr_bricks.sort();
			curr_bricks.remove(i);
			settle_bricks(&mut curr_bricks)
		}).collect::<Vec<i32>>();

	// Part 1
	println!("{}", moved_bricks.iter().filter(|&x| x == &0).count());
	// Part 2
	println!("{}", moved_bricks.iter().sum::<i32>());
}

fn main() {
	problem();
}