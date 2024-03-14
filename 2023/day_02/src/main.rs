use std::fs::read_to_string;

fn input_reader(path: String) -> Vec<String> {
    read_to_string(path).unwrap()
                        .lines()
                        .map(String::from)
                        .collect()
}

fn first_part() {
    let iterator: Vec<String> = input_reader("./input/input.txt".to_string());
    let mut ids_sum = 0;
    let mut curr_id = 1;
    let mut possible_game = true;

    iterator.iter()
            .for_each(|x| {
                let games: Vec<String> = x.split(": ")
                                          .map(|x| x.to_string())
                                          .collect();
                let mut curr_numb = String::new();
                let mut num = 0;

                games[1].replace(";", ",")
                        .chars()
                        .for_each(|g| {
                            match g {
                                'g' => {
                                    if num > 13 { possible_game = false };
                                    num = 0;
                                }
                                'b' => {
                                    if num > 14 { possible_game = false };
                                    num = 0;
                                }
                                'r' => {
                                    if num > 12 { possible_game = false };
                                    num = 0;
                                }
                                ' ' => num = if curr_numb.len() > 0 { curr_numb.parse().unwrap() } else { 0 },
                                '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' => curr_numb.push(g),
                                ',' => curr_numb = Default::default(),
                                _ => (),
                            }
                        });
                ids_sum += if possible_game { curr_id } else { 0 };
                curr_id += 1;
                possible_game = true;
            });
    println!("{}", ids_sum);
}

fn second_part() {
    let iterator: Vec<String> = input_reader("./input/input.txt".to_string());
    let mut power_set_sum = 0;

    iterator.iter()
            .for_each(|x| {
                let games: Vec<String> = x.split(": ")
                                          .map(|x| x.to_string())
                                          .collect();

                let mut min_blue = 0;
                let mut min_red = 0;
                let mut min_green = 0;
                let mut curr_numb = String::new();
                let mut num = 0;

                games[1].replace(";", ",")
                        .chars()
                        .for_each(|g| {
                            match g {
                                'g' => {
                                    if num >= min_green { min_green = num; }
                                    num = 0;
                                }
                                'b' => {
                                    if num >= min_blue { min_blue = num; }
                                    num = 0;
                                }
                                'r' => {
                                    if num >= min_red { min_red = num; }
                                    num = 0;
                                }
                                ' ' => num = if curr_numb.len() > 0 { curr_numb.parse().unwrap() } else { 0 },
                                '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' => curr_numb.push(g),
                                ',' => curr_numb = Default::default(),
                                _ => (),
                            }
                        });
                power_set_sum += min_green * min_blue * min_red;
            });
    println!("{}", power_set_sum);
}

fn main() {
    first_part();
    second_part();
}