use std::collections::HashSet;

fn s2b(i: &str, d: &str) -> usize {
    let mut r = 0;

    for c in i.chars() {
        r = r * d.len() + d.find(c).expect("a digit");
    }

    r
}

fn solve_p1(i: &str) -> usize {
    i.lines().map(|l| {
        let (sr, sc) = l.split_at(7);

        let row = s2b(sr, "FB");
        let col = s2b(sc, "LR");

        row * 8 + col
    }).max().expect("at least one")
}

fn solve_p2(i: &str) -> usize {
    let ids: HashSet<usize> = i.lines().map(|l| {
        let (sr, sc) = l.split_at(7);

        let row = s2b(sr, "FB");
        let col = s2b(sc, "LR");

        row * 8 + col
    }).collect();

    // brude force all the space
    for n in 1..(128 * 8) {
        if !ids.contains(&(n - 1)) { continue }
        if !ids.contains(&(n + 1)) { continue }
        if  ids.contains(&n) { continue }

        return n
    }

    panic!("No solution?")
}

fn main() {
    let e = std::fs::read_to_string("example").expect("example");
    let i = std::fs::read_to_string("input").expect("input");
    println!("P1 e: {:?}", solve_p1(&e));
    println!("P1 i: {:?}", solve_p1(&i));
    //println!("P2 e: {:?}", solve_p2(&e));
    println!("P2 i: {:?}", solve_p2(&i));
}
