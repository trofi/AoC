use std::collections::HashMap;

fn solve_p1(i: &str) -> String {
    let iv: Vec<Vec<char>> =
        i.lines()
         .map(|l| l.chars().collect())
         .collect();

    let mut r = String::new();

    for col in 0..iv[0].len() {
        let mut m: HashMap<char, usize> = HashMap::new();
        for row in 0..iv.len() {
            m.entry(iv[row][col])
             .and_modify(|v| *v += 1 )
             .or_insert(1);
        }
        let c = m.drain().map(|(k,v)| (v, k)).max().expect("at least one").1;
        r.push(c);
    }

    r
}

fn solve_p2(i: &str) -> String {
    let iv: Vec<Vec<char>> =
        i.lines()
         .map(|l| l.chars().collect())
         .collect();

    let mut r = String::new();

    for col in 0..iv[0].len() {
        let mut m: HashMap<char, usize> = HashMap::new();
        for row in 0..iv.len() {
            m.entry(iv[row][col])
             .and_modify(|v| *v += 1 )
             .or_insert(1);
        }
        let c = m.drain().map(|(k,v)| (v, k)).min().expect("at least one").1;
        r.push(c);
    }

    r
}

fn main() {
    let e = std::fs::read_to_string("example").expect("example");
    let i = std::fs::read_to_string("input").expect("input");
    println!("P1 example: {}", solve_p1(&e));
    println!("P1 ans: {}", solve_p1(&i));
    println!("P2 example: {}", solve_p2(&e));
    println!("P2 ans: {}", solve_p2(&i));
}
