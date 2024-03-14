use std::fs::read_to_string;
enum CardinalPoint {
    NORTH,
    WEST,
    SOUTH,
    EAST,
}
fn read_line_by_line(path: &str, separator: char) -> Vec<String> {
    read_to_string(path)
        .unwrap()
        .split(separator)
        .map(|x| x.trim())
        .map(str::to_string)
        .collect()
}
fn read_to_2d(path: &str) -> Vec<Vec<char>> {
    let iterator = read_line_by_line(path, '\n');
    let mut grid: Vec<Vec<char>> = vec![];
    iterator.iter().for_each(|i| {
        grid.push(i.chars().collect());
    });
    grid
}
fn score(columns: &Vec<String>) {
    let mut total_score = 0;
    columns.iter().for_each(|c| {
        let mut curr_score = c.len() + 1;

        c.chars().for_each(|p| {
            curr_score -= 1;
            match p {
                'O' => {
                    total_score += curr_score;
                }
                _ => (),
            }
        });
    });
    println!("{}", total_score);
}
fn platform_tilt(grid: &Vec<Vec<char>>, direction: CardinalPoint) -> Vec<Vec<char>> {
    let mut next_grid = grid.clone();
    let rows = next_grid.len();
    let cols = next_grid.iter().nth(0).unwrap().len();

    loop {
        let old_grid = next_grid.clone();
        for i in 0..rows {
            for j in 0..cols {
                if next_grid[i][j] == '#' || next_grid[i][j] == '.' {
                    continue;
                }
                match direction {
                    CardinalPoint::NORTH => {
                        if i != 0 && next_grid[i - 1][j] == '.' {
                            next_grid[i - 1][j] = 'O';
                            next_grid[i][j] = '.';
                        }
                    }
                    CardinalPoint::WEST => {
                        if j != 0 && next_grid[i][j - 1] == '.' {
                            next_grid[i][j - 1] = 'O';
                            next_grid[i][j] = '.';
                        }
                    }
                    CardinalPoint::SOUTH => {
                        if i != rows - 1 && next_grid[i + 1][j] == '.' {
                            next_grid[i + 1][j] = 'O';
                            next_grid[i][j] = '.';
                        }
                    }
                    CardinalPoint::EAST => {
                        if j != cols - 1 && next_grid[i][j + 1] == '.' {
                            next_grid[i][j + 1] = 'O';
                            next_grid[i][j] = '.';
                        }
                    }
                }
            }
        }
        if old_grid == next_grid {
            break;
        }
    }
    return next_grid;
}
fn cycle(grid: &Vec<Vec<char>>) -> Vec<Vec<char>> {
    let mut next_grid = grid.clone();
    next_grid = platform_tilt(&next_grid, CardinalPoint::NORTH);
    next_grid = platform_tilt(&next_grid, CardinalPoint::WEST);
    next_grid = platform_tilt(&next_grid, CardinalPoint::SOUTH);
    next_grid = platform_tilt(&next_grid, CardinalPoint::EAST);
    return next_grid
}
fn problem(variant: usize) {
    let mut grid = read_to_2d("./input/input.txt");
    if variant == 1 {
        grid = platform_tilt(&grid, CardinalPoint::NORTH);
    } else {
        let mut grids: Vec<Vec<Vec<char>>> = vec![grid.clone()];
        let req_cyc = 1000000000;
        for i in 0..req_cyc {
            grid = cycle(&grid);

            // Cycle detected
            if grids.contains(&grid) {
                let start = grids.iter().position(|x| x == &grid).unwrap();
                let cycle_length = i - start + 1;
                let todo = (req_cyc - i - 1) % cycle_length;

                for _i in 0..todo {
                    grid = cycle(&grid)
                }
                break;
            } else {
                grids.push(grid.clone());
            }
        }
    }
    let by_row: Vec<String> = grid.into_iter().map(|v| v.into_iter().collect()).collect();
    let row_length = by_row.iter().nth(0).unwrap().len();
    let columns = (0..row_length)
        .map(|c| {
            by_row
                .iter()
                .map(|r| r.chars().nth(c).unwrap())
                .collect::<String>()
        })
        .collect::<Vec<String>>();
    score(&columns);
}
fn main() {
    problem(1);
    problem(2);
}