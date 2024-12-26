pub(crate)
use std::fs::read_to_string;
use std::path::PathBuf;

/// Receive a path leading to a file and return a vec of string (rows of file) separated by a specified char.
///
/// Each string will be individually trimmed.
pub fn read_line_by_line(path: &str, separator: char) -> Vec<String> {
    let current_file_path = PathBuf::from(file!());
    let current_dir = current_file_path.parent().unwrap();
    let file_path = current_dir.join(path);
    read_to_string(file_path).unwrap().split(separator).map(|x| x.trim()).map(str::to_string).collect()
}

/// Receive a path leading to a file and return a vec of string (columns of file).
///
/// Each string will be individually trimmed.
pub fn read_column_by_column(path: &str) -> Vec<String> {
    let row_length = read_line_by_line(path, '\n').iter().nth(0).unwrap().len();
    let rows = read_line_by_line(path, '\n');
    (0..row_length).map(|c| rows.iter().map(|r| r.chars().nth(c).unwrap()).collect::<String>()).collect::<Vec<String>>()
}

/// Receive a path leading to a file and return a 2D vec of char (in a string).
///
/// Each string will be individually trimmed.
pub fn read_to_2d(path: &str) -> Vec<Vec<char>> {
    let iterator = read_line_by_line(path, '\n');
    let mut grid: Vec<Vec<char>> = vec![];
    iterator.iter().for_each(|i| {
        grid.push(i.chars().collect());
    });
    grid
}