use std::fs::read_to_string;
use itertools::Itertools;
use z3::ast::{Ast, Int};

#[derive(Debug)]
struct Hailstone {
	px: f64,
	py: f64,
	pz: f64,
	vx: f64,
	vy: f64,
	vz: f64,
}

pub fn read_line_by_line(path: &str, separator: char) -> Vec<String> {
	read_to_string(path).unwrap().split(separator).map(|x| x.trim()).map(str::to_string).collect()
}

fn problem_solver(stones: &Vec<Hailstone>, variant: i32) -> i64 {
	let (lb, ub): (f64, f64) = (200000000000000f64, 400000000000000f64);
	let mut result = 0;

	if variant == 1 {
		for i in 0..stones.len() {
			for j in i + 1..stones.len() {
				let a = stones.iter().nth(i).unwrap();
				let b = stones.iter().nth(j).unwrap();
				let mut valid = true;

				// Create first equation (y = m_a*x+q_a)
				let m_a = a.vy / a.vx;
				let q_a = a.py - m_a * a.px;

				// Create second equation (y = m_b*x+q_b)
				let m_b = b.vy / b.vx;
				let q_b = b.py - m_b * b.px;

				let mut intersect_x = 0f64;
				let mut intersect_y = 0f64;
				if m_a - m_b != 0f64 {
					intersect_x = (q_b - q_a) / (m_a - m_b);
					intersect_y = ((m_a / (m_a - m_b)) * (q_b - q_a)) + q_a;
				} else { valid = false; }

				let fut_ax = f64::signum(intersect_x - a.px) == f64::signum(a.vx);
				let fut_ay = f64::signum(intersect_y - a.py) == f64::signum(a.vy);
				let fut_bx = f64::signum(intersect_x - b.px) == f64::signum(b.vx);
				let fut_by = f64::signum(intersect_y - b.py) == f64::signum(b.vy);
				if !fut_ax || !fut_ay || !fut_bx || !fut_by { valid = false; }

				if intersect_x >= lb && intersect_y >= lb && intersect_x <= ub && intersect_y <= ub && valid {
					result += 1;
				}
			}
		}
		return result;
	} else {
		let context = z3::Context::new(&z3::Config::new());
		let solver = z3::Solver::new(&context);
		// Add starting coordinates of the rock and starting velocity parameters
		let [rx, ry, rz, rvx, rvy, rvz, t0, t1, t2] =
			["rx", "ry", "rz", "rvx", "rvy", "rvz", "t0", "t1", "t2"].map(|var| Int::new_const(&context, var));

		// The first three points are sufficient to guarantee only one solution coherent with the rest of the system
		solver.assert(&((&rx + &rvx * &t0)._eq(&(Int::from_i64(&context, stones[0].px as i64) + Int::from_i64(&context, stones[0].vx as i64) * &t0))));
		solver.assert(&((&ry + &rvy * &t0)._eq(&(Int::from_i64(&context, stones[0].py as i64) + Int::from_i64(&context, stones[0].vy as i64) * &t0))));
		solver.assert(&((&rz + &rvz * &t0)._eq(&(Int::from_i64(&context, stones[0].pz as i64) + Int::from_i64(&context, stones[0].vz as i64) * &t0))));

		solver.assert(&((&rx + &rvx * &t1)._eq(&(Int::from_i64(&context, stones[1].px as i64) + Int::from_i64(&context, stones[1].vx as i64) * &t1))));
		solver.assert(&((&ry + &rvy * &t1)._eq(&(Int::from_i64(&context, stones[1].py as i64) + Int::from_i64(&context, stones[1].vy as i64) * &t1))));
		solver.assert(&((&rz + &rvz * &t1)._eq(&(Int::from_i64(&context, stones[1].pz as i64) + Int::from_i64(&context, stones[1].vz as i64) * &t1))));

		solver.assert(&((&rx + &rvx * &t2)._eq(&(Int::from_i64(&context, stones[2].px as i64) + Int::from_i64(&context, stones[2].vx as i64) * &t2))));
		solver.assert(&((&ry + &rvy * &t2)._eq(&(Int::from_i64(&context, stones[2].py as i64) + Int::from_i64(&context, stones[2].vy as i64) * &t2))));
		solver.assert(&((&rz + &rvz * &t2)._eq(&(Int::from_i64(&context, stones[2].pz as i64) + Int::from_i64(&context, stones[2].vz as i64) * &t2))));

		/*
		It translates to (r = rock, hs = hailstone):

		rx + t0 * rvx == hs0x + t0 * hs0vx,
		ry + t0 * rvy == hs0y + t0 * hs0vy,
		rz + t0 * rvz == hs0z + t0 * hs0vz,

		rx + t1 * rvx == hs1x + t1 * hs1vx,
		ry + t1 * rvy == hs1y + t1 * hs1vy,
		rz + t1 * rvz == hs1z + t1 * hs1vz,

		rx + t2 * rvx == hs2x + t2 * hs2vx,
		ry + t2 * rvy == hs2y + t2 * hs2vy,
		rz + t2 * rvz == hs2z + t0 * hs2vz,
		*/

		// Assert if the model is satisfied
		assert_eq!(solver.check(), z3::SatResult::Sat);
		let model = solver.get_model().unwrap();
		result = model.eval(&(&rx + &ry + &rz), true)
		              .unwrap()
		              .as_i64()
		              .unwrap();
		return result;
	}
}

fn problem(variant: i32) {
	let iterator = read_line_by_line("./input/input.txt", '\n');
	let mut stones: Vec<Hailstone> = vec![];

	iterator.iter().for_each(|row| {
		let components = row.split(" @ ").map(|x| x.trim()).collect::<Vec<&str>>();
		let coords: (f64, f64, f64) = components[0].split(", ")
		                                           .map(|val| val.parse::<f64>().unwrap())
		                                           .collect_tuple()
		                                           .unwrap();
		let velocity: (f64, f64, f64) = components[1].split(", ")
		                                             .map(|s| s.trim())
		                                             .map(|val| val.parse::<f64>().unwrap())
		                                             .collect_tuple()
		                                             .unwrap();
		stones.push(Hailstone { px: coords.0, py: coords.1, pz: coords.2, vx: velocity.0, vy: velocity.1, vz: velocity.2 });
	});

	println!("{:?}", problem_solver(&stones, variant));
}

fn main() {
	problem(1);
	problem(2);
}