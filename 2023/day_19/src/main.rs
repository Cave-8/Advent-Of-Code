use std::fs::read_to_string;
use std::sync::Mutex;
use lazy_static::lazy_static;

lazy_static! {
    static ref COMBINATIONS: Mutex<i64> = Mutex::new(0);
}

#[derive(Debug, Clone)]
struct Part {
	x: i64,
	m: i64,
	a: i64,
	s: i64,
}

#[derive(Debug, Clone)]
struct Workflow {
	label: String,
	rules: Vec<Rule>,
	default: String,
}

#[derive(Debug, Clone)]
struct Rule {
	param: char,
	log: char,
	num: i64,
	next: String,
}

impl Part {
	pub fn new(x: i64, m: i64, a: i64, s: i64) -> Self {
		Self { x, m, a, s }
	}
}

impl Workflow {
	pub fn new(label: String, rules: Vec<Rule>, default: String) -> Self {
		Self { label, rules, default }
	}
}

impl Rule {
	pub fn new(param: char, log: char, num: i64, next: String) -> Self {
		Self { param, log, num, next }
	}
}

fn compare(p1: i64, p2: i64, log: char) -> bool {
	match log {
		'>' => if p1 > p2 { return true; } else { false }
		'<' => if p1 < p2 { return true; } else { false }
		_ => true
	}
}

fn workflow_output(part: &Part, workflow: &Workflow) -> String {
	let rules = workflow.clone().rules;
	for r in rules {
		match r.param {
			'x' => if compare(part.x, r.num, r.log) { return r.next; },
			'm' => if compare(part.m, r.num, r.log) { return r.next; },
			'a' => if compare(part.a, r.num, r.log) { return r.next; },
			's' => if compare(part.s, r.num, r.log) { return r.next; },
			_ => {}
		}
	}
	return workflow.clone().default;
}

fn workflow_output_ranges(ranges: &Vec<(i64, i64)>, workflows: &Vec<Workflow>, workflow: &Workflow) {
	let rules = workflow.clone().rules;
	let mut remaining_range = ranges.clone();

	for r in rules {
		let mut index = 0;
		let curr_range = remaining_range.clone();
		let mut analyzed_range = remaining_range.clone();
		match r.param {
			'x' => index = 0,
			'm' => index = 1,
			'a' => index = 2,
			's' => index = 3,
			_ => (),
		}
		match r.log {
			'<' => if r.num < curr_range[index].0 { continue; } else {
				analyzed_range[index] = (curr_range[index].0, r.num - 1);
				remaining_range[index] = (r.num, curr_range[index].1);
			}
			'>' => if r.num > analyzed_range[index].1 { continue; } else {
				analyzed_range[index] = (r.num + 1, curr_range[index].1);
				remaining_range[index] = (curr_range[index].0, r.num);
			}
			_ => (),
		}

		match r.next.as_str() {
			"A" => *COMBINATIONS.lock().unwrap() += analyzed_range.iter().map(|(a, b)| b - a + 1).product::<i64>(),
			"R" => (),
			_ => {
				let mut index = 0;
				workflows.iter()
				         .enumerate()
				         .for_each(|(i, x)| if x.label == r.next { index = i });
				workflow_output_ranges(&analyzed_range, workflows, workflows.iter().nth(index).unwrap());
			}
		}
	}
	match workflow.default.as_str() {
		"A" => *COMBINATIONS.lock().unwrap() += remaining_range.iter().map(|(a, b)| b - a + 1).product::<i64>(),
		"R" => (),
		_ => {
			let mut index = 0;
			workflows.iter()
			         .enumerate()
			         .for_each(|(i, x)| if x.label == workflow.default.as_str() { index = i });
			workflow_output_ranges(&remaining_range.clone(), workflows, workflows.iter().nth(index).unwrap());
		}
	}
	return;
}

fn sum_of_attributes(part: &Part) -> i64 {
	return part.s + part.x + part.m + part.a;
}

fn read_line_by_line(path: &str, separator: char) -> Vec<String> {
	read_to_string(path).unwrap().split(separator).map(|x| x.trim()).map(str::to_string).collect()
}

fn problem(variant: i32) {
	let mut iterator = read_line_by_line("./input/input.txt", '\n');
	let mut index = 0;
	iterator.iter()
	        .enumerate()
	        .for_each(|(i, x)| if x == "" { index = i; });

	let mut parts_str = iterator.split_off(index);
	parts_str.remove(0);

	let mut workflows: Vec<Workflow> = vec![];
	let mut input_pos = 0;
	let mut parts: Vec<Part> = vec![];

	// Collect workflows
	iterator.iter()
	        .for_each(|x| {
		        let mut str = x.clone();
		        str.pop();
		        let mut input = str.replace("{", " ")
		                           .replace(",", " ")
		                           .split(" ")
		                           .map(str::to_string)
		                           .collect::<Vec<String>>();
		        let label = input.remove(0);
		        let default = input.pop().unwrap();
		        let mut temp_rules = vec![];

		        for mut s in input {
			        let c = s.remove(0);
			        let log = s.remove(0);
			        let mut rest = s.split(":").map(str::to_string).collect::<Vec<String>>();
			        let num = rest.remove(0).parse::<i64>().unwrap();
			        let next = rest.remove(0);

			        let rul = Rule::new(c, log, num, next);
			        temp_rules.push(rul);
		        }

		        let workflow = Workflow::new(label, temp_rules, default);
		        workflows.push(workflow);
	        });
	workflows.iter()
	         .enumerate()
	         .for_each(|(i, x)| if x.label == "in" { input_pos = i; });

	// Collect parts
	parts_str.iter().for_each(|x| {
		let mut str = x.clone();
		str.remove(0);
		str.remove(str.len() - 1);
		let coords = str.split(",").map(str::to_string).collect::<Vec<String>>();
		let input_param = coords.iter()
		                        .map(|c| c.split("=")
		                                  .map(str::to_string).collect::<Vec<String>>()
		                                  .iter()
		                                  .nth(1)
		                                  .unwrap()
		                                  .parse::<i64>()
		                                  .unwrap())
		                        .collect::<Vec<i64>>();
		parts.push(Part::new(input_param[0], input_param[1], input_param[2], input_param[3]));
	});

	if variant == 1 {
		let mut accepted: Vec<Part> = vec![];
		parts.iter().for_each(|part| {
			let mut curr_state: String = "in".to_string();
			let mut pos = input_pos;
			let p = part.clone();

			loop {
				if curr_state != "in" && curr_state != "R" && curr_state != "A" {
					workflows.iter().enumerate().for_each(|(i, x)| if x.label == curr_state { pos = i; });
				}
				if curr_state == "R" || curr_state == "A" { break; }

				curr_state = workflow_output(&p, &workflows.iter().nth(pos).unwrap());
			}
			if curr_state == "A" { accepted.push(part.clone()); }
		});
		println!("{}", accepted.iter().map(|x| sum_of_attributes(x)).sum::<i64>())
	} else {
		let ranges: Vec<(i64, i64)> = vec![(1, 4000), (1, 4000), (1, 4000), (1, 4000)];
		workflow_output_ranges(&ranges, &workflows, workflows.iter().nth(input_pos).unwrap());
		println!("{}", COMBINATIONS.lock().unwrap());
	}
}

fn main() {
	problem(1);
	problem(2);
}