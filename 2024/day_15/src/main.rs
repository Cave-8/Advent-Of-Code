use std::collections::HashSet;
use utils::*;

type Coord = (i64, i64);

fn parse_problem1(input: &Vec<String>) -> (HashSet<Coord>, HashSet<Coord>, Vec<Coord>, Coord) {
    let mut boxes: HashSet<Coord> = HashSet::new();
    let mut walls: HashSet<Coord> = HashSet::new();
    let mut moves: Vec<Coord> = Vec::new();
    let mut start_pos: Coord = (0, 0);

    for (row, line) in input.iter().enumerate() {
        if line.contains('#') {
            for (col, c) in line.chars().enumerate() {
                if c == 'O' {
                    boxes.insert((row as i64, col as i64));
                } else if c == '@' {
                    start_pos = (row as i64, col as i64);
                } else if c == '#' {
                    walls.insert((row as i64, col as i64));
                }
            }
        } else if line.contains('^')
            || line.contains('>')
            || line.contains('<')
            || line.contains('v')
        {
            for c in line.chars() {
                match c {
                    '^' => moves.push((-1, 0)),
                    '>' => moves.push((0, 1)),
                    'v' => moves.push((1, 0)),
                    '<' => moves.push((0, -1)),
                    _ => (),
                }
            }
        }
    }
    (boxes, walls, moves, start_pos)
}

fn problem1(
    boxes: &HashSet<Coord>,
    walls: &HashSet<Coord>,
    moves: &Vec<Coord>,
    start_pos: &Coord,
) -> i64 {
    let mut movable_boxes = boxes.clone();
    let (mut x, mut y) = start_pos.clone();

    for (dx, dy) in moves {
        let (nx, ny) = (x + dx, y + dy);
        // Hit a wall
        if walls.contains(&(nx, ny)) {
            continue;
        }
        // Hit a box
        if movable_boxes.contains(&(nx, ny)) {
            let mut boxes_in_the_way: HashSet<Coord> = HashSet::from([(nx, ny)]);
            let mut not_involved_boxes: HashSet<Coord> = HashSet::new();
            let (mut curr_x, mut curr_y) = (nx, ny);
            loop {
                // Hit a wall during scan
                if walls.contains(&(curr_x + dx, curr_y + dy)) {
                    break;
                }
                // Found a box
                if movable_boxes.contains(&(curr_x + dx, curr_y + dy)) {
                    boxes_in_the_way.insert((curr_x + dx, curr_y + dy));
                    (curr_x, curr_y) = (curr_x + dx, curr_y + dy);
                    continue;
                }
                // No wall and no box
                break;
            }
            // Boxes not involved
            not_involved_boxes = movable_boxes
                .difference(&boxes_in_the_way)
                .cloned()
                .collect();
            // Increase the coordinates of the boxes to be moved
            let mut transformed_boxes: HashSet<Coord> = boxes_in_the_way
                .into_iter()
                .map(|(r, c)| (r + dx, c + dy))
                .collect();
            // If the intersection is empty then the transformation has been successful
            if transformed_boxes.intersection(&not_involved_boxes).count() == 0
                && transformed_boxes.intersection(&walls).count() == 0
            {
                movable_boxes = transformed_boxes
                    .union(&not_involved_boxes)
                    .cloned()
                    .collect();
                (x, y) = (nx, ny);
                continue;
            }
            continue;
        }
        // Free cell
        (x, y) = (nx, ny)
    }
    // GPS
    movable_boxes.into_iter().map(|(r, c)| 100 * r + c).sum()
}

fn parse_problem2(input: &Vec<String>) -> (HashSet<Coord>, HashSet<Coord>, Vec<Coord>, Coord) {
    let mut new_input: Vec<String> = Vec::new();
    for line in input.iter() {
        let mut curr_row = "".to_string();
        for c in line.chars() {
            match c {
                '#' => curr_row.push_str("##"),
                'O' => curr_row.push_str("[]"),
                '.' => curr_row.push_str(".."),
                '@' => curr_row.push_str("@."),
                '^' => curr_row.push_str("^"),
                '>' => curr_row.push_str(">"),
                'v' => curr_row.push_str("v"),
                '<' => curr_row.push_str("<"),
                _ => (),
            }
        }
        new_input.push(curr_row);
    }

    let mut boxes: HashSet<Coord> = HashSet::new();
    let mut walls: HashSet<Coord> = HashSet::new();
    let mut moves: Vec<Coord> = Vec::new();
    let mut start_pos: Coord = (0, 0);

    for (row, line) in new_input.iter().enumerate() {
        if line.contains('#') {
            for (col, c) in line.chars().enumerate() {
                if c == '[' {
                    boxes.insert((row as i64, col as i64));
                } else if c == '@' {
                    start_pos = (row as i64, col as i64);
                } else if c == '#' {
                    walls.insert((row as i64, col as i64));
                }
            }
        } else if line.contains('^')
            || line.contains('>')
            || line.contains('<')
            || line.contains('v')
        {
            for c in line.chars() {
                match c {
                    '^' => moves.push((-1, 0)),
                    '>' => moves.push((0, 1)),
                    'v' => moves.push((1, 0)),
                    '<' => moves.push((0, -1)),
                    _ => (),
                }
            }
        }
    }
    (boxes, walls, moves, start_pos)
}

fn problem2(
    boxes: &HashSet<Coord>,
    walls: &HashSet<Coord>,
    moves: &Vec<Coord>,
    start_pos: &Coord,
) -> i64 {
    let mut movable_boxes = boxes.clone();
    let (mut x, mut y) = start_pos.clone();

    for (dx, dy) in moves {
        let (nx, ny) = (x + dx, y + dy);

        // Hit a wall
        if walls.contains(&(nx, ny)) {
            continue;
        }

        // Hit a box
        if movable_boxes.contains(&(nx, ny)) || movable_boxes.contains(&(nx, ny - 1)) {
            let mut to_visit: Vec<Coord> = if movable_boxes.contains(&(nx, ny)) {
                Vec::from([(nx, ny)])
            } else if movable_boxes.contains(&(nx, ny - 1)) {
                Vec::from([(nx, ny - 1)])
            } else {
                Vec::new()
            };
            let mut exec_move = true;
            let mut visited: HashSet<Coord> = HashSet::new();

            while !to_visit.is_empty() {
                let (curr_x, curr_y) = to_visit.pop().unwrap();
                let (nx, ny) = (curr_x + dx, curr_y + dy);

                // Already visited, branches pruning
                if visited.contains(&(curr_x, curr_y)) {
                    continue;
                }
                visited.insert((curr_x, curr_y));

                // Boxes hit a wall
                if walls.contains(&(nx, ny)) || walls.contains(&(nx, ny + 1)) {
                    exec_move = false;
                    break;
                }

                if movable_boxes.contains(&(nx, ny)) {
                    to_visit.push((nx, ny));
                }
                if movable_boxes.contains(&(nx, ny - 1)) {
                    to_visit.push((nx, ny - 1));
                }
                if movable_boxes.contains(&(nx, ny + 1)) {
                    to_visit.push((nx, ny + 1));
                }
            }

            if !exec_move {
                continue;
            }

            (x, y) = (nx, ny);
            let mut temp_boxes: HashSet<Coord> = HashSet::new();
            for b in movable_boxes.iter() {
                if visited.contains(b) {
                    temp_boxes.insert((b.0 + dx, b.1 + dy));
                    continue;
                }
                temp_boxes.insert((b.0, b.1));
            }
            movable_boxes = temp_boxes;
            continue;
        }

        // Free cell
        (x, y) = (nx, ny)
    }
    // GPS
    movable_boxes.into_iter().map(|(r, c)| 100 * r + c).sum()
}

fn main() {
    let input = read_line_by_line("../../day_15/input/input.txt", '\n');

    let (boxes, walls, moves, start_pos) = parse_problem1(&input);
    let problem1 = problem1(&boxes, &walls, &moves, &start_pos);
    println!("{}", problem1);

    let (boxes, walls, moves, start_pos) = parse_problem2(&input);
    let problem2 = problem2(&boxes, &walls, &moves, &start_pos);
    println!("{}", problem2);
}
