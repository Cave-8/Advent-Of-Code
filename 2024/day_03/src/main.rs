use regex::Regex;
use utils::*;

#[derive(Debug)]
struct Mul {
    a: i32,
    b: i32,
}

fn problem1(input: &String) -> i32 {
    let re = Regex::new(r"mul\((\d+),(\d+)\)").unwrap();
    let mut muls: Vec<Mul> = vec![];
    for mat in re.captures_iter(input) {
        let a = mat
            .get(1)
            .map_or("", |m| m.as_str())
            .parse::<i32>()
            .unwrap();
        let b = mat
            .get(2)
            .map_or("", |m| m.as_str())
            .parse::<i32>()
            .unwrap();
        muls.push(Mul { a, b });
    }
    muls.iter().map(|x| x.a * x.b).sum::<i32>()
}

fn problem2(input: &String) -> i32 {
    let re = Regex::new(r"mul\((\d+),(\d+)\)|do\(\)|don't\(\)").unwrap();
    let mut elements: Vec<String> = vec![];
    for mat in re.captures_iter(input) {
        match (mat.get(1), mat.get(2), mat.get(0)) {
            (Some(a), Some(b), _) => {
                elements.push(format!("{}, {}", a.as_str(), b.as_str()));
            }
            (_, _, Some(full_match)) if full_match.as_str() == "do()" => {
                elements.push("do()".to_string());
            }
            (_, _, Some(full_match)) if full_match.as_str() == "don't()" => {
                elements.push("don't()".to_string());
            }
            _ => {}
        }
    }

    let mut valid = true;
    let mut muls: Vec<Mul> = vec![];
    for el in elements {
        if el == "don't()" {
            valid = false;
            continue;
        }
        if el == "do()" {
            valid = true;
            continue;
        }
        if valid {
            let nums: Vec<&str> = el.split(", ").collect();
            muls.push(Mul {
                a: nums[0].parse::<i32>().unwrap(),
                b: nums[1].parse::<i32>().unwrap(),
            });
        }
    }
    muls.iter().map(|x| x.a * x.b).sum::<i32>()
}

fn main() {
    // Single line input
    let input = read_line_by_line("../../day_03/input/input.txt", '\n');

    let problem1: i32 = problem1(&input[0]);
    let problem2: i32 = problem2(&input[0]);

    println!("{:?}", problem1);
    println!("{:?}", problem2);
}
