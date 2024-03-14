use std::fs::read_to_string;
use std::collections::BinaryHeap;

// Can be optimized and rewritten
fn read_line_by_line(path: &str, separator: char) -> Vec<String> {
	read_to_string(path).unwrap().split(separator).map(|x| x.trim()).map(str::to_string).collect()
}

fn read_to_2d(path: &str) -> Vec<Vec<char>> {
	let iterator = read_line_by_line(path, '\n');
	let mut grid: Vec<Vec<char>> = vec![];
	iterator.iter().for_each(|i| {
		grid.push(i.chars().collect());
	});
	grid
}

fn shortest_path(grid: &Vec<Vec<char>>, max: usize) -> i32 {
	let mut costs: Vec<Vec<i32>> = vec![];
	let mut visited: Vec<(usize, usize, (isize, isize), usize)> = vec![];
	let mut cell_dist: BinaryHeap<(i32, usize, usize, (isize, isize), usize)> = BinaryHeap::new();
	let mut to_ret = 0;

	for i in 0..grid.len() {
		costs.push(grid[i].iter().map(|x| *x as i32 - 48).collect());
	}

	cell_dist.push((0, 0, 0, (0, 0), 0));

	while !cell_dist.is_empty() {
		let curr_cell = cell_dist.pop().unwrap();

		if curr_cell.1 == grid.len() - 1 && curr_cell.2 == grid[0].len() - 1 {
			to_ret = -curr_cell.0;
			break;
		}

		if visited.contains(&(curr_cell.1, curr_cell.2, curr_cell.3, curr_cell.4)) { continue; }
		visited.push((curr_cell.1, curr_cell.2, curr_cell.3, curr_cell.4));

		if curr_cell.4 < max && (curr_cell.1, curr_cell.2) != (0, 0) {
			let x = (curr_cell.1 as isize + (1 * curr_cell.3.0)) as usize;
			let y = (curr_cell.2 as isize + (1 * curr_cell.3.1)) as usize;

			if x >= 0 && y >= 0 && x <= grid.len() - 1 && y <= grid[0].len() - 1 {
				cell_dist.push((curr_cell.0 - costs[x][y], x, y, curr_cell.3, curr_cell.4 + 1));
			}
		}

		for (rc, cc) in [(-1, 0), (0, 1), (1, 0), (0, -1)] {
			if (rc, cc) == (curr_cell.3.0, curr_cell.3.1) || (rc, cc) == (-curr_cell.3.0, -curr_cell.3.1) {
				continue;
			}

			let x = (curr_cell.1 as isize + (1 * rc)) as usize;
			let y = (curr_cell.2 as isize + (1 * cc)) as usize;

			if x >= 0 && y >= 0 && x <= grid.len() - 1 && y <= grid[0].len() - 1 {
				cell_dist.push((curr_cell.0 - costs[x][y], x, y, (rc, cc), 1));
			}
		}
	}
	return to_ret;
}

fn shortest_path_2(grid: &Vec<Vec<char>>, max: usize) -> i32 {
	let mut costs: Vec<Vec<i32>> = vec![];
	let mut visited: Vec<(usize, usize, (isize, isize), usize)> = vec![];
	let mut cell_dist: BinaryHeap<(i32, usize, usize, (isize, isize), usize)> = BinaryHeap::new();
	let mut to_ret = 0;

	for i in 0..grid.len() {
		costs.push(grid[i].iter().map(|x| *x as i32 - 48).collect());
	}

	cell_dist.push((0, 0, 0, (0, 0), 0));

	while !cell_dist.is_empty() {
		let curr_cell = cell_dist.pop().unwrap();

		if curr_cell.1 == grid.len() - 1 && curr_cell.2 == grid[0].len() - 1 && curr_cell.4 >= 4 {
			to_ret = -curr_cell.0;
			break;
		}

		if visited.contains(&(curr_cell.1, curr_cell.2, curr_cell.3, curr_cell.4)) { continue; }
		visited.push((curr_cell.1, curr_cell.2, curr_cell.3, curr_cell.4));

		if curr_cell.4 < max && (curr_cell.1, curr_cell.2) != (0, 0) {
			let x = (curr_cell.1 as isize + (1 * curr_cell.3.0)) as usize;
			let y = (curr_cell.2 as isize + (1 * curr_cell.3.1)) as usize;

			if x >= 0 && y >= 0 && x <= grid.len() - 1 && y <= grid[0].len() - 1 {
				cell_dist.push((curr_cell.0 - costs[x][y], x, y, curr_cell.3, curr_cell.4 + 1));
			}
		}

		if curr_cell.4 >= 4 || (curr_cell.1, curr_cell.2) == (0, 0) {
			for (rc, cc) in [(-1, 0), (0, 1), (1, 0), (0, -1)] {
				if (rc, cc) == (curr_cell.3.0, curr_cell.3.1) || (rc, cc) == (-curr_cell.3.0, -curr_cell.3.1) {
					continue;
				}

				let x = (curr_cell.1 as isize + (1 * rc)) as usize;
				let y = (curr_cell.2 as isize + (1 * cc)) as usize;

				if x >= 0 && y >= 0 && x <= grid.len() - 1 && y <= grid[0].len() - 1 {
					cell_dist.push((curr_cell.0 - costs[x][y], x, y, (rc, cc), 1));
				}
			}
		}
	}
	return to_ret;
}

fn main() {
	let grid = read_to_2d("./input/input.txt");
	println!("{}", shortest_path(&grid, 3));
	println!("{}", shortest_path_2(&grid, 10));
}
