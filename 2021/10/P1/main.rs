use std::{*, fs::*, io::*};

fn get_input(input_file: &str) -> Vec<String> {
    let r = BufReader::new(File::open(input_file).unwrap());

    return r.lines()
            .map(|l| l.unwrap())
            .collect();
}

fn solve(l: &String) -> usize {
    let mut stack = Vec::new();
    for c in l.chars() {
        // expect push, always valid:
        match c { '(' | '[' | '{' | '<' => { stack.push(c); continue; }, _ => (), }

        // nothing to compare pop against
        if stack.len() == 0 { return 0; }

        match (stack.pop().unwrap(), c) {
            // expect matching pop
            ('(', ')') | ('[', ']') | ('{', '}') | ('<', '>') => (),
            // first mismatch
            (_, ')') => return 3,
            (_, ']') => return 57,
            (_, '}') => return 1197,
            (_, '>') => return 25137,
            v => { println!("unexpected state: {:?}", v); unreachable!() }
        }

    }
    return 0;
}

fn main() {
    for input_file in ["example", "input"] {
        let input = get_input(input_file);

        let ans : usize = input.iter().map(|l| solve(l)).sum();

        println!("{}: {:?}", input_file, ans);
    }
}
