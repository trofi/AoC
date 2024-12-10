use std::collections::HashSet;

fn solve_p1(i: &str) -> isize {
    i.lines().map(|e| e.parse::<isize>().expect("a number")).sum()
}

fn solve_p2(i: &str) -> isize {
    let mut seen: HashSet<isize> = HashSet::new();
    let mut f: isize = 0;

    for l in i.lines().cycle() {
        let d: isize = l.parse::<isize>().expect("a number");

        f += d;
        if seen.contains(&f) { return f }
        seen.insert(f);
    }

    panic!("Unstable");
}

fn main() {
    let e = std::fs::read_to_string("example").expect("example");
    let i = std::fs::read_to_string("input").expect("input");
    println!("P1 e: {:?}", solve_p1(&e));
    println!("P1 i: {:?}", solve_p1(&i));
    println!("P2 e: {:?}", solve_p2(&e));
    println!("P2 i: {:?}", solve_p2(&i));
}
