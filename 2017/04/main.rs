use std::collections::BTreeSet;

fn solve_p1(i: &str) -> usize {
    i.lines().filter(|l| {
        let wv: Vec<_> = l.split(" ").collect();
        let ws: BTreeSet<_> = wv.iter().collect();

        wv.len() == ws.len()
    }).count()
}

fn solve_p2(i: &str) -> usize {
    i.lines().filter(|l| {
        let wv: Vec<_> = l.split(" ").collect();
        let ws: BTreeSet<_> = wv.iter().map(|w|{
            let mut cv: Vec<char> = w.chars().collect();
            cv.sort();
            cv
        }).collect();

        wv.len() == ws.len()
    }).count()
}

fn main() {
    let i = std::fs::read_to_string("input").expect("input");
    println!("P1 ans: {}", solve_p1(&i));
    println!("P2 ans: {}", solve_p2(&i));
}
