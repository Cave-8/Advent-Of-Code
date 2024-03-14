use std::fs::read_to_string;

fn input_reader(path: String) -> Vec<String> {
    read_to_string(path).unwrap()
                        .lines()
                        .map(String::from)
                        .collect()
}

fn first_part() {
    let iterator = input_reader("./input/input.txt".to_string());
    let mut final_score = 0;

    for mut i in iterator {
        let mut current_score = 0;
        let mut found_numbers = 0;
        let mut winning_numbers: Vec<i32> = Default::default();
        let mut building_number = "".to_string();
        let mut new_winner = 0;
        let mut looking_for_winner = false;

        i.push_str(" \n");
        let parts: Vec<&str> = i.split(':').collect();
        for c in parts[1].chars() {
            match c {
                ' ' => {
                    if building_number.len() > 0 {
                        new_winner = building_number.parse().unwrap();
                        match looking_for_winner {
                            true => {
                                if winning_numbers.contains(&new_winner) {
                                    found_numbers += 1;
                                    current_score = if found_numbers != 1 { 2 * current_score } else { 1 };
                                }
                            }
                            false => winning_numbers.push(new_winner),
                        }
                    }
                    building_number = "".to_string();
                }
                '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' => building_number.push(c),

                '|' => looking_for_winner = true,
                _ => ()
            }
            new_winner = 0;
        }
        final_score += current_score;
    }
    println!("{}", final_score);
}

fn second_part() {
    let iterator = input_reader("./input/input.txt".to_string());
    let mut cards_numb: Vec<(usize, usize)> = Default::default();
    let prob_size = iterator.len();
    let mut total_cards = 0;

    for i in 0..prob_size {
        cards_numb.push((i, 1));
    }

    for i in 0..prob_size {
        let card_id: usize = cards_numb[i].0;
        let num_matches: usize = cards_numb[i].1;
        let mut curr_card = iterator[card_id].clone();
        let mut num_copies = 0;
        let mut winning_numbers: Vec<i32> = Default::default();
        let mut building_number = "".to_string();
        let mut new_winner = 0;
        let mut looking_for_winner = false;

        total_cards += num_matches;
        curr_card.push_str(" \n");

        let parts: Vec<&str> = curr_card.split(':').collect();

        for c in parts[1].chars() {
            match c {
                ' ' => {
                    if building_number.len() > 0 {
                        new_winner = building_number.parse().unwrap();
                        match looking_for_winner {
                            true => num_copies += if winning_numbers.contains(&new_winner) { 1 } else { 0 },
                            false => winning_numbers.push(new_winner),
                        }
                    }
                    building_number = "".to_string();
                }
                '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' => building_number.push(c),

                '|' => looking_for_winner = true,
                _ => ()
            }
            new_winner = 0;
        }

        for i in card_id..=(card_id + (num_copies)) {
            cards_numb[i].1 += if i > card_id && i < prob_size { num_matches } else { 0 };
        }
    }
    println!("{}", total_cards);
}

fn main() {
    first_part();
    second_part();
}
