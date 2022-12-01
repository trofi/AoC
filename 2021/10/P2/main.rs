use std::{*, fs::*, io::*, iter::*};

fn get_input(input_file: &str) -> Vec<String> {
    let r = BufReader::new(File::open(input_file).unwrap());

    return r.lines().map(|l| l.unwrap()).collect();
}

fn solve(l: &String) -> usize {
    let mut stack = Vec::new();
    for c in l.chars() {
        match c { '(' | '[' | '{' | '<' => { stack.push(c); continue; }, _ => (), }

        if stack.len() == 0 { return 0; }

        match (stack.pop().unwrap(), c) {
            ('(', ')') | ('[', ']') | ('{', '}') | ('<', '>') => (),
            _ => return 0
        }

    }

    stack.reverse();

    let mut score = 0;
    for v in stack.iter() {
        score = score * 5 + match v {
            '(' => 1, '[' => 2, '{' => 3, '<' => 4,
            _ => unreachable!()
        }
    }

    return score;
}

fn main() {
    for input_file in ["example", "input"] {
        let input = get_input(input_file);

        let mut scores = Vec::from_iter(
            input.iter()
                 .map(|l| solve(l))
                 .filter(|e| *e != 0));
        scores.sort();
        let ans = scores[scores.len() / 2];

        println!("{}: {}", input_file, ans);
    }
}
