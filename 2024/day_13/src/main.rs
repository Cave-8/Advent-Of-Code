use regex::Regex;
use utils::read_line_by_line;

#[derive(Debug)]
struct Machine {
    a: (i64, i64),
    b: (i64, i64),
    prize: (i64, i64),
}

fn parse_machines(input: &Vec<String>) -> Vec<Machine> {
    let filtered_input: Vec<_> = input.iter().filter(|s| *s != "").collect();
    let machines_input: Vec<_> = filtered_input
        .chunks(3)
        .map(|chunk| chunk.to_vec())
        .collect();
    let mut machines: Vec<Machine> = Vec::new();

    for m in machines_input.iter() {
        let re_buttons = Regex::new(r"X\+(\d+), Y\+(\d+)").unwrap();
        let re_prize = Regex::new(r"X=(\d+), Y=(\d+)").unwrap();

        let Some(a) = re_buttons.captures(m[0]) else {
            panic!()
        };
        let a_x = a[1].parse::<i64>().unwrap();
        let a_y = a[2].parse::<i64>().unwrap();

        let Some(b) = re_buttons.captures(m[1]) else {
            panic!()
        };
        let b_x = b[1].parse::<i64>().unwrap();
        let b_y = b[2].parse::<i64>().unwrap();

        let Some(a) = re_prize.captures(m[2]) else {
            panic!()
        };
        let prize_x = a[1].parse::<i64>().unwrap();
        let prize_y = a[2].parse::<i64>().unwrap();

        machines.push(Machine {
            a: (a_x, a_y),
            b: (b_x, b_y),
            prize: (prize_x, prize_y),
        });
    }
    machines
}

fn problem(machines: &Vec<Machine>) -> i64 {
    /*
        We have to solve the equations (first example):
        94A + 22B = 8400
        34A + 67B = 5400

        Since (a1, a2) = (94, 34), (b1, b2) = (22, 67), (c1, c2) = (8400, 5400) then it's the following system of equations:
        a1A + b1B = c1
        a2A + b2B = c2

        We can use Cramer, before we check that:
        a1b2 - b1a2 != 0
        if so we return 0, then we check for integers solutions then if not:
        A = (c1b2-b1c2)/(a1b2-b1a2)
        B = (a1c2-c1a2)/(a1b2-b1a2)
        We return 3A + B
    */

    machines
        .iter()
        .map(|m| {
            let num1 = m.prize.0 * m.b.1 - m.b.0 * m.prize.1;
            let num2 = m.a.0 * m.prize.1 - m.prize.0 * m.a.1;
            let den = m.a.0 * m.b.1 - m.b.0 * m.a.1;

            if m.a.0 * m.b.1 - m.b.0 * m.a.1 == 0 {
                0
            } else if ((num1 as f64) / (den as f64)).fract() != 0.0 {
                0
            } else if ((num2 as f64) / (den as f64)).fract() != 0.0 {
                0
            } else {
                (3 * (num1 / den)) + (num2 / den)
            }
        })
        .sum()
}

fn main() {
    let input = read_line_by_line("../../day_13/input/input.txt", '\n');
    let machines = parse_machines(&input);

    let problem1 = problem(&machines);
    println!("Problem 1: {}", problem1);

    let machine_adj: Vec<Machine> = machines
        .iter()
        .map(|m| Machine {
            a: (m.a.0, m.a.1),
            b: (m.b.0, m.b.1),
            prize: (m.prize.0 + 10000000000000, m.prize.1 + 10000000000000),
        })
        .collect();
    let problem2 = problem(&machine_adj);
    println!("Problem 2: {}", problem2);
}
