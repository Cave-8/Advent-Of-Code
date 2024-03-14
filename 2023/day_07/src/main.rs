use std::collections::HashMap;
use std::fs::read_to_string;
use strum::IntoEnumIterator;
use strum_macros::EnumIter;
use itertools::{Itertools};
use crate::TYPE::{FiveOfAKind, FourOfAKind, FullHouse, HighCard, OnePair, ThreeOfAKind, TwoPair};

#[derive(Debug, PartialEq, EnumIter)]
enum TYPE {
	HighCard,
	OnePair,
	TwoPair,
	ThreeOfAKind,
	FullHouse,
	FourOfAKind,
	FiveOfAKind,
}

fn input_reader(path: String) -> String { read_to_string(path).unwrap() }

// 1 for first part, 2 for second part
fn camel_sort(s: &mut Vec<(String, i32)>, variant: i32) {
	let mut order = HashMap::new();
	let alphabet_1 = vec!['A', 'K', 'Q', 'J', 'T', '9', '8', '7', '6', '5', '4', '3', '2'];
	let alphabet_2 = vec!['A', 'K', 'Q', 'T', '9', '8', '7', '6', '5', '4', '3', '2', 'J'];

	if variant == 1 {
		let mut index = 0;
		alphabet_1.iter().for_each(|a| {
			order.insert(a, index);
			index += 1;
		});
	} else {
		let mut index = 0;
		alphabet_2.iter().for_each(|a| {
			order.insert(a, index);
			index += 1;
		});
	}
	s.sort_by_key(|s| {
		let mut key = Vec::new();
		s.0.chars().for_each(|c| {
			key.push(order.get(&c)
			              .unwrap_or(&usize::MAX));
		});
		key
	});
}

// variant = 1 for first part, 2 for second part
fn type_assigner(ranking: &mut Vec<(TYPE, HashMap<char, i32>, String, i32)>, hands: &Vec<(String, i32)>, variant: i32) {
	for h in hands {
		let original_card = h.0.clone();
		// Only in second part
		let modified_card;

		let mut chars: HashMap<char, i32> = Default::default();
		// Only in second part this is different from chars
		let mut modified_chars: HashMap<char, i32> = Default::default();
		h.0.chars().for_each(|x| {
			chars.insert(x, 1 + if chars.contains_key(&x) { chars[&x] } else { 0 });
		});

		if variant == 1 || original_card == "JJJJJ" {
			modified_chars = chars
		} else {
			let sub = chars.iter()
			               .filter(|(a, _)| **a != 'J')
			               .reduce(|a, b| if a.1 > b.1 { a } else { b })
			               .unwrap();
			modified_card = original_card.replace('J', &(sub.0.to_string()));

			modified_card.chars().for_each(|x| {
				modified_chars.insert(x, 1 + if modified_chars.contains_key(&x) { modified_chars[&x] } else { 0 });
			});
		}

		// Type assignment
		match modified_chars.len() {
			// Five of a kind
			1 => ranking.push((FiveOfAKind, modified_chars, h.0.clone(), h.1.clone())),
			// Four of a kind or FullHouse
			2 => {
				if modified_chars.iter().find_map(|(k, &v)| if v == 4 { Some(k) } else { None }) != None {
					ranking.push((FourOfAKind, modified_chars, h.0.clone(), h.1.clone()));
				} else {
					ranking.push((FullHouse, modified_chars, h.0.clone(), h.1.clone()));
				}
			}
			// Three of a kind or Two pair
			3 => {
				if modified_chars.iter().find_map(|(k, &v)| if v == 3 { Some(k) } else { None }) != None {
					ranking.push((ThreeOfAKind, modified_chars, h.0.clone(), h.1.clone()));
				} else {
					ranking.push((TwoPair, modified_chars, h.0.clone(), h.1.clone()));
				}
			}
			// One pair
			4 => ranking.push((OnePair, modified_chars, h.0.clone(), h.1.clone())),
			// High card
			5 => ranking.push((HighCard, modified_chars, h.0.clone(), h.1.clone())),
			_ => ()
		}
	}
}

fn score_assigner(ranking: &mut Vec<(TYPE, HashMap<char, i32>, String, i32)>, bid_score: &mut Vec<(i32, i32)>, variant: i32) {
	let mut curr_counter = 1;
	for t in TYPE::iter() {
		let mut to_order: Vec<(String, i32)> = ranking.iter()
		                                              .filter(|x| x.0 == t)
		                                              .map(|(_, _, z, a)| (z.clone(), a.clone()))
		                                              .clone()
		                                              .collect();
		camel_sort(&mut to_order, variant);
		let mut scores: Vec<i32> = Default::default();
		to_order.iter().for_each(|s| {
			scores.push(s.1);
		});
		scores.iter().rev().collect::<Vec<&i32>>().iter().for_each(|s| {
			bid_score.push((**s, curr_counter));
			curr_counter += 1;
		});
	}
}

fn problem(variant: i32) {
	let iterator: Vec<String> = input_reader("./input/input.txt".to_string())
		.split("\n")
		.map(str::to_string)
		.collect();
	let mut hands: Vec<(String, i32)> = Default::default();
	iterator.iter().for_each(|i| {
		let tuple: (String, String) = i
			.trim()
			.split(" ")
			.map(str::to_string)
			.collect_tuple()
			.unwrap();
		hands.push((tuple.0, tuple.1.parse().unwrap()));
	});
	let mut ranking: Vec<(TYPE, HashMap<char, i32>, String, i32)> = Default::default();
	type_assigner(&mut ranking, &hands, variant);
	let mut bid_score: Vec<(i32, i32)> = Default::default();
	score_assigner(&mut ranking, &mut bid_score, variant);

	let result: i32 = bid_score.iter().map(|(a, b)| a * b).sum();
	println!("{}", result);
}

fn main() {
	problem(1);
	problem(2);
}