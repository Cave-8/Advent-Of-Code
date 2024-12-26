use utils::*;

#[derive(Debug, Clone, Eq, Ord, PartialEq, PartialOrd)]
struct Space {
    id: usize,
    start: usize,
    end: usize,
    size: usize,
}

fn parser(input: &Vec<String>) -> (Vec<Space>, Vec<Space>) {
    let mut free_spaces: Vec<Space> = Vec::new();
    let mut file_spaces: Vec<Space> = Vec::new();
    let mut current_index = 0usize;
    let mut file_index = 0usize;
    for (index, char) in input[0].chars().enumerate() {
        let int = char.to_digit(10).unwrap() as usize;
        if index % 2 == 0 {
            file_spaces.push(Space {
                id: file_index,
                start: current_index,
                end: current_index + int - 1,
                size: int,
            });
            file_index += 1;
        } else {
            if int != 0 {
                free_spaces.push(Space {
                    id: usize::MAX,
                    start: current_index,
                    end: current_index + int - 1,
                    size: int,
                });
            }
        }
        current_index += int;
    }
    (free_spaces, file_spaces)
}

fn is_compact(free_space: &Vec<Space>, file_space: &Vec<Space>) -> bool {
    let leftmost_free_space = free_space
        .get(find_leftmost_space(&free_space))
        .unwrap()
        .clone();
    let rightmost_file_space = file_space
        .get(find_rightmost_space(&file_space))
        .unwrap()
        .clone();
    leftmost_free_space.start == rightmost_file_space.end + 1
}

fn find_leftmost_space(spaces: &Vec<Space>) -> usize {
    let mut min_index = 0usize;
    let mut min_start = usize::MAX;
    for (index, space) in spaces.iter().enumerate() {
        if min_start > space.start {
            min_start = space.start;
            min_index = index;
        }
    }
    min_index
}

fn find_rightmost_space(spaces: &Vec<Space>) -> usize {
    let mut max_index = 0usize;
    let mut max_start = 0usize;
    for (index, space) in spaces.iter().enumerate() {
        if max_start < space.start {
            max_start = space.start;
            max_index = index;
        }
    }
    max_index
}

// Only for part 2
fn find_leftmost_big_enough_space(spaces: &Vec<Space>, size: usize) -> Option<usize> {
    let mut ordered_spaces = spaces.clone();
    ordered_spaces.sort_by(|a, b| a.start.cmp(&b.start));
    for (ind, ord) in ordered_spaces.iter().enumerate() {
        if ord.size >= size {
            return Some(ind);
        }
    }
    None
}

fn get_checksum(spaces: &mut Vec<Space>) -> usize {
    let mut checksum = 0;
    for space in spaces.iter() {
        for index in space.start..=space.end {
            checksum += index * space.id;
        }
    }
    checksum
}

fn problem1(free_space: &mut Vec<Space>, file_space: &mut Vec<Space>) -> usize {
    while !is_compact(free_space, file_space) {
        let leftmost_free_space = free_space
            .get(find_leftmost_space(&free_space))
            .unwrap()
            .clone();
        let rightmost_file_space = file_space
            .get(find_rightmost_space(&file_space))
            .unwrap()
            .clone();

        // Free space is large enough
        if leftmost_free_space.size >= rightmost_file_space.size {
            // It is the free space substituted with file space
            let moved_file_space = Space {
                id: rightmost_file_space.id,
                start: leftmost_free_space.start,
                end: leftmost_free_space.start + rightmost_file_space.size - 1,
                size: rightmost_file_space.size,
            };

            // Replace leftmost free space
            let leftmost_index = find_leftmost_space(&free_space);
            free_space[leftmost_index] = Space {
                id: usize::MAX,
                start: leftmost_free_space.start + rightmost_file_space.size,
                end: leftmost_free_space.end,
                size: leftmost_free_space.size - rightmost_file_space.size,
            };

            // Move file space
            let rightmost_index = find_rightmost_space(&file_space);
            file_space.remove(rightmost_index);
            file_space.push(moved_file_space);

            // Replace file space with empty size
            free_space.push(Space {
                id: usize::MAX,
                start: rightmost_file_space.start,
                end: rightmost_file_space.end,
                size: rightmost_file_space.size,
            });
        }
        // Free space is too small -> it will be deleted
        else {
            // It is the filled empty space with replaced id
            let new_rightmost_file_space = Space {
                id: rightmost_file_space.id,
                start: leftmost_free_space.start,
                end: leftmost_free_space.end,
                size: leftmost_free_space.size,
            };

            // Insert new file space
            file_space.push(new_rightmost_file_space);

            // Replace rightmost file space
            let rightmost_index = find_rightmost_space(&file_space);
            file_space[rightmost_index] = Space {
                id: rightmost_file_space.id,
                start: rightmost_file_space.start,
                end: rightmost_file_space.end - leftmost_free_space.size,
                size: rightmost_file_space.size - leftmost_free_space.size,
            };

            // Replace file space with empty size
            free_space.push(Space {
                id: usize::MAX,
                start: rightmost_file_space.end - leftmost_free_space.size + 1,
                end: rightmost_file_space.end,
                size: leftmost_free_space.size,
            });

            // Remove first free space
            let leftmost_index = find_leftmost_space(&free_space);
            let _ = free_space.remove(leftmost_index);
        }

        // Delete empty blocks
        let mut empty_free_spaces = free_space
            .iter()
            .enumerate()
            .filter(|(_, f)| f.size == 0)
            .map(|(i, _)| i)
            .collect::<Vec<usize>>();
        let mut empty_file_spaces = file_space
            .iter()
            .enumerate()
            .filter(|(_, f)| f.size == 0)
            .map(|(i, _)| i)
            .collect::<Vec<usize>>();
        empty_free_spaces.sort_by(|a, b| b.cmp(a));
        empty_file_spaces.sort_by(|a, b| b.cmp(a));
        for index in empty_free_spaces {
            if index < free_space.len() {
                free_space.remove(index);
            }
        }
        for index in empty_file_spaces {
            if index < free_space.len() {
                file_space.remove(index);
            }
        }
    }
    get_checksum(file_space)
}

fn problem2(free_space: &mut Vec<Space>, file_space: &mut Vec<Space>) -> usize {
    let movements = file_space.len();
    for m in (0..movements).rev() {
        // It is the space with currently analyzed id
        let current_id_space = file_space
            .iter()
            .filter(|x| x.id == m)
            .collect::<Vec<_>>()
            .get(0)
            .unwrap()
            .clone();
        // No space is big enough if it's none
        let biggest_free_space =
            match find_leftmost_big_enough_space(free_space, current_id_space.size) {
                Some(index) => index,
                None => continue,
            };
        let leftmost_free_space = free_space.get(biggest_free_space).unwrap().clone();

        if leftmost_free_space.size >= current_id_space.size
            && leftmost_free_space.start < current_id_space.start
        {
            // It is the free space substituted with file space
            let moved_file_space = Space {
                id: current_id_space.id,
                start: leftmost_free_space.start,
                end: leftmost_free_space.start + current_id_space.size - 1,
                size: current_id_space.size,
            };

            // Replace leftmost free space
            let leftmost_index =
                match find_leftmost_big_enough_space(&free_space, current_id_space.size) {
                    Some(index) => index,
                    None => continue, // Should not happen
                };
            free_space[leftmost_index] = Space {
                id: usize::MAX,
                start: leftmost_free_space.start + current_id_space.size,
                end: leftmost_free_space.end,
                size: leftmost_free_space.size - current_id_space.size,
            };

            // Move file space
            let previous_file_space = current_id_space.clone();
            let biggest_id_index = file_space.iter().position(|x| x.id == m).unwrap();
            file_space.remove(biggest_id_index);
            file_space.push(moved_file_space);

            // Replace file space with empty size
            free_space.push(Space {
                id: usize::MAX,
                start: previous_file_space.start,
                end: previous_file_space.end,
                size: previous_file_space.size,
            });
        }

        // Delete empty blocks
        let mut empty_free_spaces = free_space
            .iter()
            .enumerate()
            .filter(|(_, f)| f.size == 0)
            .map(|(i, _)| i)
            .collect::<Vec<usize>>();
        let mut empty_file_spaces = file_space
            .iter()
            .enumerate()
            .filter(|(_, f)| f.size == 0)
            .map(|(i, _)| i)
            .collect::<Vec<usize>>();
        empty_free_spaces.sort_by(|a, b| b.cmp(a));
        empty_file_spaces.sort_by(|a, b| b.cmp(a));
        for index in empty_free_spaces {
            if index < free_space.len() {
                free_space.remove(index);
            }
        }
        for index in empty_file_spaces {
            if index < free_space.len() {
                file_space.remove(index);
            }
        }
    }
    // Checksum
    get_checksum(file_space)
}

fn main() {
    let input = read_line_by_line("../../day_09/input/input.txt", '\n');

    let (mut free_spaces, mut file_spaces) = parser(&input);
    let problem1 = problem1(&mut free_spaces, &mut file_spaces);
    println!("{}", problem1);

    let (mut free_spaces, mut file_spaces) = parser(&input);
    let problem2 = problem2(&mut free_spaces, &mut file_spaces);
    println!("{}", problem2);
}
