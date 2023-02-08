type Pos = (isize, isize); // (x, y)

const DIRS: [Pos; 4] = [
    (0, -1), (-1, 0), (0, 1), (1, 0),
];

fn get_d(c: &str, d: usize) -> usize {
    match c {
        "L" => (d + DIRS.len() - 1) % DIRS.len(),
        "R" => (d + 1) % DIRS.len(),
        _ => panic!("Unknown direction {}", c)
    }
}

fn add((lx, ly): &mut Pos, (rx, ry): Pos) {
    *lx += rx;
    *ly += ry;
}

fn solve_p1(i: &str) -> isize {
    let mut p: Pos = (0, 0);
    let mut dir: usize = 0;

    for e in i.trim().split(", ") {
        let (d, n) = e.split_at(1);
        dir = get_d(d, dir);
        let s: isize = n.parse().expect("number");

        for _ in 0..s {
            add(&mut p, DIRS[dir]);
        }
    }
    p.0.abs() + p.1.abs()
}

fn solve_p2(i: &str) -> isize {
    let mut p: Pos = (0, 0);
    let mut dir: usize = 0;

    use std::collections::BTreeSet;
    let mut visited: BTreeSet<Pos> = BTreeSet::new();
    visited.insert(p);

    for e in i.trim().split(", ") {
        let (d, n) = e.split_at(1);
        dir = get_d(d, dir);
        let s: isize = n.parse().expect("number");

        for _ in 0..s {
            add(&mut p, DIRS[dir]);
            if !visited.insert(p) {
                return p.0.abs() + p.1.abs()
            }
        }
    }

    todo!("No duplicates?")
}


fn main() {
    let i = std::fs::read_to_string("input").expect("input");
    println!("P1 ans: {}", solve_p1(&i));
    println!("P2 ans: {}", solve_p2(&i));
}
