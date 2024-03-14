use std::fs::read_to_string;
use lazy_static::lazy_static;
use std::sync::Mutex;

lazy_static! {
    static ref VISITED: Mutex<Vec<(usize, usize)>> = Mutex::new(Vec::new());
	static ref SEEN: Mutex<Vec<(usize, usize, char)>> = Mutex::new(Vec::new());
	static ref TOVISIT: Mutex<Vec<(usize, usize, char)>> = Mutex::new(Vec::new());
}

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

fn traverse_grid(grid: &Vec<Vec<char>>, curr_row: usize, curr_col: usize, direction: char) -> i32 {
	let rows = grid.len();
	let cols = grid.iter().nth(0).unwrap().len();
	let mut energized = 0;
	let mut state: (usize, usize, char) = (curr_row, curr_col, direction);
	let mut end = true;
	TOVISIT.lock().unwrap().push((curr_row, curr_col, direction));

	loop {
		if TOVISIT.lock().unwrap().is_empty() { break; } else if end {
			state = TOVISIT.lock().unwrap()[0];
			end = false;
		}

		if !VISITED.lock().unwrap().contains(&(state.0, state.1)) {
			VISITED.lock().unwrap().push((state.0, state.1));
			energized += 1;
		}
		if !SEEN.lock().unwrap().contains(&(state.0, state.1, state.2)) {
			SEEN.lock().unwrap().push((state.0, state.1, state.2))
		} else {
			TOVISIT.lock().unwrap().remove(0);
			end = true;
			continue;
		}

		match grid[state.0][state.1] {
			'.' => {
				match state.2 {
					'^' => if state.0 == 0 {
						TOVISIT.lock().unwrap().remove(0);
						end = true;
					} else { state.0 -= 1; },
					'>' => if state.1 == cols - 1 {
						TOVISIT.lock().unwrap().remove(0);
						end = true;
					} else { state.1 += 1; },
					'v' => if state.0 == rows - 1 {
						TOVISIT.lock().unwrap().remove(0);
						end = true;
					} else { state.0 += 1; },
					'<' => if state.1 == 0 {
						TOVISIT.lock().unwrap().remove(0);
						end = true;
					} else { state.1 -= 1; },
					_ => ()
				}
			}
			'|' => {
				match state.2 {
					'^' => if state.0 == 0 {
						TOVISIT.lock().unwrap().remove(0);
						end = true;
					} else { state.0 -= 1; },
					'>' | '<' => {
						if state.0 != 0 && state.0 != rows - 1 {
							TOVISIT.lock().unwrap().push((state.0 + 1, state.1, 'v'));
							TOVISIT.lock().unwrap().push((state.0 - 1, state.1, '^'));
							end = true;
						} else if state.0 == 0 {
							state.0 += 1;
							state.2 = 'v';
						} else if state.0 == rows - 1 {
							state.0 -= 1;
							state.2 = '^';
						}
					}
					'v' => if state.0 == rows - 1 {
						TOVISIT.lock().unwrap().remove(0);
						end = true;
					} else { state.0 += 1; },
					_ => ()
				}
			}
			'-' => {
				match state.2 {
					'>' => if state.1 == cols - 1 {
						TOVISIT.lock().unwrap().remove(0);
						end = true;
					} else {
						state.1 += 1;
					},
					'^' | 'v' => {
						if state.1 != 0 && state.1 != cols - 1 {
							TOVISIT.lock().unwrap().push((state.0, state.1 + 1, '>'));
							TOVISIT.lock().unwrap().push((state.0, state.1 - 1, '<'));
							end = true;
						} else if state.1 == 0 {
							state.1 += 1;
							state.2 = '>';
						} else if state.1 == cols - 1 {
							state.1 -= 1;
							state.2 = '<';
						};
					}
					'<' => if state.1 == 0 {
						TOVISIT.lock().unwrap().remove(0);
						end = true;
					} else { state.1 -= 1; },
					_ => ()
				}
			}
			'/' => {
				match state.2 {
					'^' => if state.1 == cols - 1 {
						TOVISIT.lock().unwrap().remove(0);
						end = true;
					} else {
						state.1 += 1;
						state.2 = '>';
					},
					'>' => if state.0 == 0 {
						TOVISIT.lock().unwrap().remove(0);
						end = true;
					} else {
						state.0 -= 1;
						state.2 = '^';
					},
					'v' => if state.1 == 0 {
						TOVISIT.lock().unwrap().remove(0);
						end = true;
					} else {
						state.1 -= 1;
						state.2 = '<';
					},
					'<' => if state.0 == rows - 1 {
						TOVISIT.lock().unwrap().remove(0);
						end = true;
					} else {
						state.0 += 1;
						state.2 = 'v';
					},
					_ => ()
				}
			}
			'\\' => {
				match state.2 {
					'^' => if state.1 == 0 {
						TOVISIT.lock().unwrap().remove(0);
						end = true;
					} else {
						state.1 -= 1;
						state.2 = '<';
					}
					'>' => if state.0 == rows - 1 {
						TOVISIT.lock().unwrap().remove(0);
						end = true;
					} else {
						state.0 += 1;
						state.2 = 'v';
					},
					'v' => if state.1 == cols - 1 {
						TOVISIT.lock().unwrap().remove(0);
						end = true;
					} else {
						state.1 += 1;
						state.2 = '>';
					},
					'<' => if state.0 == 0 {
						TOVISIT.lock().unwrap().remove(0);
						end = true;
					} else {
						state.0 -= 1;
						state.2 = '^';
					},
					_ => ()
				}
			}
			_ => ()
		}
	}
	VISITED.lock().unwrap().clear();
	SEEN.lock().unwrap().clear();
	TOVISIT.lock().unwrap().clear();
	return energized;
}

fn first_part() {
	let grid = read_to_2d("./input/input.txt");
	println!("{}", traverse_grid(&grid, 0, 0, '>'));
}

// Can be improved by adding further memoization for light beams
fn second_part() {
	let grid = read_to_2d("./input/input.txt");
	traverse_grid(&grid, 0, 0, '>');
	let mut starting_points: Vec<(usize, usize, char)> = vec![];
	for i in 0..grid.len() {
		for j in 0..grid.iter().nth(0).unwrap().len() {
			if i == 0 { starting_points.push((i, j, 'v')); } else if j == 0 { starting_points.push((i, j, '>')); } else if i == grid.len() - 1 { starting_points.push((i, j, '^')); } else if j == grid.iter().nth(0).unwrap().len() - 1 { starting_points.push((i, j, '<')); }
		}
	}
	println!("{}", starting_points.iter()
	                              .map(|(r, c, d)| traverse_grid(&grid, r.clone(), c.clone(), d.clone()))
	                              .max()
	                              .unwrap());
}

fn main() {
	first_part();
	second_part();
}