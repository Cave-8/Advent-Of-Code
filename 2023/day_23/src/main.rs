use std::cmp::max;
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

// Keep track of paths to do multiple passages if needed
// Used also in part 2 (brute-force)
// I should have turned the matrix in a graph and contract long straight path with no intersections (and keep a weighted edge) -> NP-hard problem
fn search_path(grid: &Vec<Vec<char>>) -> usize{
	let mut col = grid.iter()
	                  .nth(0)
	                  .unwrap()
	                  .iter()
	                  .position(|&c| c == '.')
	                  .unwrap();
	let start_pos = (0usize, col);

	col = grid.iter()
	          .nth(grid.iter().len() - 1)
	          .unwrap()
	          .iter()
	          .position(|&c| c == '.')
	          .unwrap();
	let end_pos = (grid.iter().len() - 1, col);

	let mut result = 1;
	let mut stack: Vec<((usize, usize), Vec<(usize, usize)>)> = vec![(start_pos, vec![] )];

	while stack.len() != 0 {
		let (curr, mut visited) = stack.remove(stack.len() - 1);
		visited.push(curr);

		if curr == end_pos { result = max(result, visited.iter().count()); println!("{}", result - 1); };

		match grid[curr.0][curr.1] {
			'#' => continue,
			'.' => {
				for (nr, nc) in [(0, -1), (-1, 0), (0, 1), (1, 0)] {
					let new_x = (curr.0 as isize + nr) as usize;
					let new_y = (curr.1 as isize + nc) as usize;

					if new_x == usize::MAX || new_y == usize::MAX || new_x >= grid.len() || new_y >= grid[0].len() {
						continue
					}

					if grid[new_x][new_y] != '#' && !visited.contains(&(new_x, new_y)) {
						stack.push(((new_x, new_y), visited.clone()));
					}
				}
			}
			_ => {
				let mut new_x = curr.0;
				let mut new_y = curr.1;

				match grid[curr.0][curr.1] {
					'^' => new_x -= 1,
					'>' => new_y += 1,
					'v' => new_x += 1,
					'<' => new_y -= 1,
					_ => (),
				}
				if !visited.contains(&(new_x, new_y)) {
					stack.push(((new_x, new_y), visited.clone()));
				}
			}
		}
	}
	return result - 1;
}

fn problem(variant: i32) {
	let grid = read_to_2d("./input/input.txt");
	if variant == 1 {
		println!("{}", search_path(&grid));
	}
	else {
		let mut new_grid: Vec<Vec<char>> = vec![];
		grid.iter().enumerate().for_each(|(r, row)| {
			new_grid.push(row.clone());
			row.iter().enumerate().for_each(|(c, &ch)| {
				if ch == '#' {
					new_grid[r][c] = '#';
				}
				else {
					new_grid[r][c] = '.';
				}
			});
		});
		println!("{}", search_path(&new_grid));
	}
}

fn main() {
	problem(1);
	problem(2);
}