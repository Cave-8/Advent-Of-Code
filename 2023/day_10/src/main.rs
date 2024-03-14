use std::fs::read_to_string;
use itertools::Itertools;

// In the second part I used a mathematical approach:
// 1) I calculate the area using Gauss's area formula (shoelace method)
// 2) I calculate the perimeter using the method used in the first part
// 3) By using Pick's theorem we can find the number of inner nodes with inner_points = area - (perimeter/2) + 1

fn input_reader(path: String) -> String { read_to_string(path).unwrap() }

fn problem(grid: &Vec<String>, variant: i32) {
    // Finding starting point
    let mut i = 0;
    let mut j = 0;
    let mut start: (usize, usize) = (0, 0);
    for g in grid.clone() {
        j = 0;
        for c in g.chars() {
            if c == 'S' {
                start = (i, j);
                break;
            }
            j += 1;
        }
        i += 1;
    }

    let mut below: char = 'N';
    let mut right: char = 'N';
    let mut upper: char = 'N';
    let mut left: char = 'N';

    if start.0 != grid.len() - 1 { below = grid.iter().nth(start.0 + 1).unwrap().chars().nth(start.1).unwrap(); }
    if start.0 != 0 { upper = grid.iter().nth(start.0 - 1).unwrap().chars().nth(start.1).unwrap(); }
    if start.1 != grid.iter().len() - 1 { right = grid.iter().nth(start.0).unwrap().chars().nth(start.1 + 1).unwrap(); }
    if start.1 != 0 { left = grid.iter().nth(start.0).unwrap().chars().nth(start.1 - 1).unwrap(); }

    if variant == 1 {
        // Find next position
        if (left == '-' || left == 'F') && left != 'N' { println!("{}", perimeter_area(&grid, start, (start.0, start.1 - 1), start).0 / 2); } else if (upper == '|' || upper == 'F' || upper == '7') && upper != 'N' { println!("{}", perimeter_area(&grid, start, (start.0 - 1, start.1), start).0 / 2); } else if (right == '-' || right == '7' || right == 'J') && right != 'N' { println!("{}", perimeter_area(&grid, start, (start.0, start.1 + 1), start).0 / 2); } else if (below == '|' || below == 'J' || below == 'L') && below != 'N' { println!("{}", perimeter_area(&grid, start, (start.0 + 1, start.1), start).0 / 2); }
    } else {
        let mut perimeter = 0;
        let mut a = 0;
        if left == '-' || left == 'F' {
            perimeter = perimeter_area(&grid, start, (start.0, start.1 - 1), start).0;
            a = perimeter_area(&grid, start, (start.0, start.1 - 1), start).1;
        } else if upper == '|' || upper == 'F' || upper == '7' {
            perimeter = perimeter_area(&grid, start, (start.0 - 1, start.1), start).0;
            a = perimeter_area(&grid, start, (start.0 - 1, start.1), start).1;
        } else if right == '-' || right == '7' {
            perimeter = perimeter_area(&grid, start, (start.0, start.1 + 1), start).0;
            a = perimeter_area(&grid, start, (start.0, start.1 + 1), start).1;
        } else if below == '|' || below == 'J' || below == 'L' {
            perimeter = perimeter_area(&grid, start, (start.0 + 1, start.1), start).0;
            a = perimeter_area(&grid, start, (start.0 + 1, start.1), start).1;
        }
        // It depends on ordering of labels for vertexes
        if a < 0 { a = -a }
        // Pick's theorem
        println!("{}", a - (perimeter / 2) + 1);
    }
}

fn perimeter_area(grid: &Vec<String>, old_coord: (usize, usize), curr_pos: (usize, usize), start: (usize, usize)) -> (i32, i32) {
    let mut c = curr_pos.clone();
    let mut o = old_coord.clone();
    let mut points: Vec<(usize, usize)> = vec![start];
    let mut steps = 1;

    loop {
        if c.0 == start.0 && c.1 == start.1 {
            break;
        } else {
            points.push(c);
            match grid.iter().nth(c.0).unwrap().chars().nth(c.1).unwrap() {
                '|' => {
                    if c.0 == o.0 + 1 {
                        o = c;
                        c.0 += 1;
                    } else {
                        o = c;
                        c.0 -= 1;
                    }
                }
                '-' => {
                    if c.1 == o.1 + 1 {
                        o = c;
                        c.1 += 1;
                    } else {
                        o = c;
                        c.1 -= 1;
                    }
                }
                '7' => {
                    if c.1 == o.1 + 1 {
                        o = c;
                        c.0 += 1;
                    } else {
                        o = c;
                        c.1 -= 1;
                    }
                }
                'J' => {
                    if c.1 == o.1 + 1 {
                        o = c;
                        c.0 -= 1;
                    } else {
                        o = c;
                        c.1 -= 1;
                    }
                }
                'F' => {
                    if o.1 != 0 && c.1 == o.1 - 1 {
                        o = c;
                        c.0 += 1;
                    } else {
                        o = c;
                        c.1 += 1;
                    }
                }
                'L' => {
                    if o.1 != 0 && c.1 == o.1 - 1 {
                        o = c;
                        c.0 -= 1;
                    } else {
                        o = c;
                        c.1 += 1;
                    }
                }
                _ => ()
            }
            steps += 1;
        }
    }
    let area: i32 = points.iter().circular_tuple_windows().map(|(x, y)| x.0 as i32 * y.1 as i32 - x.1 as i32 * y.0 as i32).sum::<i32>() / 2;
    return (steps, area);
}

fn main() {
    let grid: Vec<String> = input_reader("./input/input.txt".to_string()).split("\n")
                                                                         .map(|x| x.trim().to_string()).collect();
    problem(&grid, 1);
    problem(&grid, 2);
}