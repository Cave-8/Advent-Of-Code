use std::process::exit;
use std::collections::HashMap;
use std::fs::read_to_string;
use lazy_static::lazy_static;
use num::integer::lcm;
use std::sync::Mutex;

lazy_static! {
	static ref CYCLES: Mutex<i64> = Mutex::new(1);
	static ref CONJS: Mutex<usize> = Mutex::new(0);
}

pub fn read_line_by_line(path: &str, separator: char) -> Vec<String> {
	read_to_string(path).unwrap().split(separator).map(|x| x.trim()).map(str::to_string).collect()
}

fn conj_routine(conjunctions: &mut HashMap<String, HashMap<String, String>>, key: &String, ops: &mut Vec<(String, Vec<String>)>, operations: &mut Vec<(String, Vec<String>)>, signals: &mut Vec<bool>, button: &i32, variant: i32) {
	let mut conj = conjunctions.get(key).unwrap().clone();
	if signals[0] { conj.insert(ops[0].clone().0, "HIGH".to_string()); } else { conj.insert(ops[0].clone().0, "LOW".to_string()); }
	conjunctions.insert(key.clone(), conj.clone());

	let op = operations.iter().position(|(a, _)| a == key).unwrap();
	if conj.iter().filter(|(_a, b)| b.clone() == &"HIGH".to_string()).count() == conj.iter().count() {
		(0..operations.iter().nth(op).unwrap().1.len()).for_each(|_| signals.push(false));
	} else {
		(0..operations.iter().nth(op).unwrap().1.len()).for_each(|_| signals.push(true));
		if variant == 2 && (key == "gp" || key == "xp" || key == "ln" || key == "xl") {
			if *CONJS.lock().unwrap() == 4 {
				println!("{:?}", *CYCLES.lock().unwrap());
				// Harsh exit, can be written better
				exit(0);
			} else {
				let curr = *CYCLES.lock().unwrap();
				*CYCLES.lock().unwrap() = lcm(curr, (button.clone() + 1) as i64);
				*CONJS.lock().unwrap() += 1;
			}
		}
	}

	ops.push(operations.iter().nth(op).unwrap().clone());
}

fn switch_ff(flip_flops: &mut HashMap<String, String>, key: &String, ops: &mut Vec<(String, Vec<String>)>, operations: &mut Vec<(String, Vec<String>)>, signals: &mut Vec<bool>) {
	if !signals[0] {
		if flip_flops.get(key).unwrap().clone() == "ON".to_string() {
			flip_flops.insert(key.clone(), "OFF".to_string());

			let op = operations.iter().position(|(a, _)| a == key).unwrap();
			(0..operations.iter().nth(op).unwrap().1.len()).for_each(|_| signals.push(false));
			ops.push(operations.iter().nth(op).unwrap().clone());
		} else {
			flip_flops.insert(key.clone(), "ON".to_string());

			let op = operations.iter().position(|(a, _)| a == key).unwrap();
			(0..operations.iter().nth(op).unwrap().1.len()).for_each(|_| signals.push(true));
			ops.push(operations.iter().nth(op).unwrap().clone());
		}
	}
}

fn machine_routine(flip_flops: &mut HashMap<String, String>, conjunctions: &mut HashMap<String, HashMap<String, String>>, operations: &mut Vec<(String, Vec<String>)>, mut high_sigs: i32, mut low_sigs: i32, button: i32, variant: i32) -> i32 {
	for b in 0..button {
		low_sigs += 1;
		let mut ops: Vec<(String, Vec<String>)> = vec![];

		for o in operations.clone() {
			if o.0 == "broadcaster" { ops.push(o.clone()); }
		}

		let mut signals = vec![];
		for _i in 0..ops.iter().nth(0).unwrap().clone().1.len() {
			signals.push(false);
		}

		while !ops.is_empty() {
			let curr_op = ops.iter().nth(0).unwrap().clone();

			for i in curr_op.1 {
				if flip_flops.contains_key(&*i) {
					switch_ff(flip_flops, &i, &mut ops, operations, &mut signals);
				} else if conjunctions.contains_key(&*i) {
					conj_routine(conjunctions, &i, &mut ops, operations, &mut signals, &b, variant);
				}
				if signals[0] { high_sigs += 1; } else { low_sigs += 1; }
				signals.remove(0);
			}
			ops.remove(0);
		}
	}
	return high_sigs * low_sigs;
}

fn problem(variant: i32) {
	let iterator: Vec<String> = read_line_by_line("./input/input.txt", '\n');
	let mut flip_flops: HashMap<String, String> = HashMap::new();
	let mut conjunctions: HashMap<String, HashMap<String, String>> = HashMap::new();
	let mut operations: Vec<(String, Vec<String>)> = vec![];

	iterator.iter().for_each(|s| {
		let label_ids = s.split(" -> ")
		                 .map(str::to_string)
		                 .collect::<Vec<String>>();
		let mut sender = label_ids.iter().nth(0).unwrap().clone();
		let receiver = label_ids.iter().nth(1).unwrap().clone();
		match sender.chars().nth(0) {
			Some('%') => {
				sender.remove(0);
				flip_flops.insert(sender.clone(), "OFF".to_string());
			}
			Some('&') => {
				sender.remove(0);
				conjunctions.insert(sender.clone(), HashMap::new());
			}
			_ => (),
		}

		if receiver.contains(", ") {
			let receivers = receiver
				.split(", ")
				.map(str::to_string)
				.collect::<Vec<String>>();
			operations.push((sender.clone(), receivers));
		} else {
			operations.push((sender.clone(), vec![receiver]));
		}
	});

	for (a, b) in operations.clone() {
		for i in b {
			if conjunctions.contains_key(&*i.clone()) {
				let mut curr_conj = conjunctions.get(&*i.clone()).unwrap().clone();
				curr_conj.insert(a.clone(), "LOW".to_string());
				conjunctions.insert((&*i).parse().unwrap(), curr_conj.clone());
			}
		}
	}

	if variant == 1 {
		println!("{}", machine_routine(&mut flip_flops.clone(), &mut conjunctions.clone(), &mut operations.clone(), 0, 0, 1000, 1));
	}
	else {
		println!("{}", machine_routine(&mut flip_flops, &mut conjunctions, &mut operations, 0, 0, i32::MAX, 2));
	}
}

fn main() {
	problem(1);
	problem(2);
}