use std::fs::read_to_string;

pub fn read_line_by_line(path: &str, separator: char) -> Vec<String> {
	read_to_string(path).unwrap().split(separator).map(|x| x.trim()).map(str::to_string).collect()
}

pub fn read_to_2d(path: &str) -> Vec<Vec<char>> {
	let iterator = read_line_by_line(path, '\n');
	let mut grid: Vec<Vec<char>> = vec![];
	iterator.iter().for_each(|i| {
		grid.push(i.chars().collect());
	});
	grid
}

// Position reached with even steps will stay in the final grid configuration
fn bfs(grid: &Vec<Vec<char>>, max_steps: usize) -> i64 {
	let mut queue: Vec<(usize, usize, usize)> = vec![];
	let mut reachable: Vec<(usize, usize)> = vec![];
	// Also contains not traversable tiles
	let mut visited: Vec<(usize, usize)> = vec![];

	grid.iter().enumerate().for_each(|(r, a)| {
		a.iter().enumerate().for_each(|(c, _b)| {
			if grid[r][c] == '#' {
				visited.push((r, c));
			} else if grid[r][c] == 'S' {
				queue.push((r, c, max_steps));
				visited.push((r, c));
			}
		});
	});

	while !queue.is_empty() {
		let curr_cell = queue.remove(0);

		if curr_cell.2 % 2 == 0 { reachable.push((curr_cell.0, curr_cell.1)) }
		if curr_cell.2 == 0 { continue; }

		for (r, c) in [(-1, 0), (0, 1), (1, 0), (0, -1)] {
			let x = (curr_cell.0 as isize + r) as usize;
			let y = (curr_cell.1 as isize + c) as usize;

			if x < 0 || y < 0 || x >= grid.len() || y >= grid[0].len() {
				continue;
			} else {
				if !visited.contains(&(x, y)) {
					visited.push((x, y));
					queue.push((x, y, curr_cell.2 - 1));
				}
			}
		}
	}

	return reachable.iter().len() as i64;
}

fn problem(variant: i32) {
	let grid = read_to_2d("./input/input.txt");

	if variant == 1 { println!("{}", bfs(&grid, 64)) };

	if variant == 2 {
		// Expand grid in order to be sufficient for three steps of the algorithm for interpolation
		let mut exp_grid: Vec<Vec<char>> = vec![];
		for _ in 0..5 {
			for r in &grid {
				let mut new_row: Vec<char> = Vec::new();
				for _ in 0..5 {
					for &c in r {
						if &c == &'S' { new_row.push('.'); } else { new_row.push(c); }
					}
				}
				exp_grid.push(new_row);
			}
		}
		let offset = 2 * grid.len() + (grid.len() - 1) / 2;
		exp_grid[offset][offset] = 'S';

		let input = 26501365;
		let starting_step = input % grid.len();
		let remaining_cycles = ((input - starting_step) / grid.len()) as i64;

		// Quadratic equation

		let y0 = bfs(&exp_grid, starting_step);
		let y1 = bfs(&exp_grid, starting_step + grid.len());
		let y2 = bfs(&exp_grid, starting_step + 2 * grid.len());

		let x0: i64 = 0;
		let x1: i64 = 1;
		let x2: i64 = 2;

		// From system of equations
		let a = ((y0 - y1) * (x0 - x2) - (y0 - y2) * (x0 - x1)) / ((x0 * x0 - x1 * x1) * (x0 - x2) - (x0 * x0 - x2 * x2) * (x0 - x1));
		let b = ((y0 - y2) * (x0 - x1) - a * (x0 * x0 - x2 * x2) * (x0 - x1)) / ((x0 - x1) * (x0 - x2));
		let c = y0 - a * x0 * x0 - b * x0;

		let result = a * remaining_cycles * remaining_cycles + b * remaining_cycles + c;
		println!("{}", result);
	}
}

fn main() {
	problem(1);
	problem(2);
}