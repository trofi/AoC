use std::collections::HashSet;

type Equ = (usize, Vec<usize>);

fn parse_eq(i: &str) -> Equ {
    let (v_s, ops_s) = i.split_once(": ").expect("val an ops");

    let v = v_s.parse::<usize>().expect("value is a number");

    let ops: Vec<usize> = ops_s
        .split(" ")
        .map(|e| e.parse::<usize>().expect("op is a number"))
        .collect();

    (v, ops)
}

fn parse_input(i: &str) -> Vec<Equ> {
    i.lines().map(|l| parse_eq(l)).collect()
}

fn is_valid(e: &Equ, fs: &[fn(usize, usize) -> usize]) -> bool {
    let mut ops: &[usize] = &e.1;
    assert!(ops.len() >= 1);

    let mut vs: HashSet<usize> = HashSet::new();

    vs.insert(ops[0]);
    ops = &ops[1..];

    while ops.len() > 0 {
        let op = ops[0];
        ops = &ops[1..];

        let mut new_vs: HashSet<usize> = HashSet::new();
        for v in &vs {
            for f in fs {
                new_vs.insert(f(*v, op));
            }
        }

        vs = new_vs;
    }

    vs.contains(&e.0)
}

fn add(l: usize, r: usize) -> usize { l + r }
fn mul(l: usize, r: usize) -> usize { l * r }
fn cat(l: usize, r: usize) -> usize { format!("{l}{r}").parse::<usize>().expect("a number") }

fn solve(i: &str, fs: &[fn(usize, usize) -> usize]) -> usize {
    let es = parse_input(i);

    es.into_iter()
      .filter(|e| is_valid(e, fs))
      .map(|e| e.0)
      .sum()
}

fn main() {
    let e = std::fs::read_to_string("example").expect("example");
    let i = std::fs::read_to_string("input").expect("input");
    println!("P1 e: {:?}", solve(&e, &[add, mul]));
    println!("P1 i: {:?}", solve(&i, &[add, mul]));
    println!("P2 e: {:?}", solve(&e, &[add, mul, cat]));
    println!("P2 i: {:?}", solve(&i, &[add, mul, cat]));
}
