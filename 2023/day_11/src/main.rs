use std::fs::read_to_string;

fn input_reader(path: String) -> String { read_to_string(path).unwrap() }

fn expand_universe(iter: &Vec<String>, expansion_rate: i64, galaxy_coords: &mut Vec<(i64, i64)>) {
    // Find, collect and sort coordinates of galaxies
    iter.iter()
        .enumerate()
        .for_each(|(i, x)|
            x.chars()
             .enumerate()
             .for_each(|(j, y)| if y == '#' { galaxy_coords.push((i as i64, j as i64)) }));
    galaxy_coords.sort();

    // Find empty rows
    let expanded_rows: Vec<usize> = iter.iter()
                                        .enumerate()
                                        .map(|g| if !g.1.contains("#") { g.0 } else { usize::MAX })
                                        .filter(|g| *g != usize::MAX)
                                        .collect::<Vec<usize>>();

    // Find empty columns
    let expanded_columns: Vec<usize> = (0..iter[0].len()).map(|c| iter.iter().map(|r| r.chars().nth(c).unwrap()).collect::<String>())
                                                         .into_iter()
                                                         .enumerate()
                                                         .map(|g| if !g.1.contains("#") { g.0 } else { usize::MAX })
                                                         .filter(|g| *g != usize::MAX)
                                                         .collect::<Vec<usize>>();

    // For each row that has expanded update row coordinates of galaxy below it
    expanded_rows.iter()
                 .enumerate()
                 .map(|(x, y)| y + x * expansion_rate as usize)
                 .into_iter()
                 .for_each(|r| {
                     galaxy_coords.iter_mut().for_each(|g| {
                         if g.0 > r as i64 { g.0 += expansion_rate }
                     })
                 });

    // For each row that has expanded update column coordinates of galaxy at the right of it
    expanded_columns.iter()
                    .enumerate()
                    .map(|(x, y)| y + x * expansion_rate as usize)
                    .into_iter()
                    .for_each(|c| {
                        galaxy_coords.iter_mut().for_each(|g| {
                            if g.1 > c as i64 { g.1 += expansion_rate }
                        })
                    });
}

fn taxi_distance(g1: &(i64, i64), g2: &(i64, i64)) -> i64 {
    return (g1.0 - g2.0).abs() + (g1.1 - g2.1).abs();
}

fn problem(expansion_rate: i64) {
    let iterator: Vec<String> = input_reader("./input/input.txt".to_string()).split("\n")
                                                                             .map(|x| x.trim().to_string())
                                                                             .collect();
    let mut galaxy_coords: Vec<(i64, i64)> = vec![];
    // Find coordinates of distanced galaxies
    expand_universe(&iterator, expansion_rate, &mut galaxy_coords);

    let mut sum: i64 = 0;
    // Calculate the distance between pairs of galaxies
    galaxy_coords.iter()
                 .enumerate()
                 .for_each(|(i, g)| galaxy_coords.iter().enumerate().for_each(|(j, h)| if j > i { sum += taxi_distance(g, h) }));
    println!("{:?}", sum);
}

fn main() {
    let exp_rate_prob1: i64 = 2;
    let exp_rate_prob2: i64 = 1000000;

    problem(exp_rate_prob1 - 1);
    problem(exp_rate_prob2 - 1);
}


