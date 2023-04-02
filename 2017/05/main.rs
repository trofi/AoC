fn solve_p1(i: &str) -> usize {
    let mut v: Vec<isize> =
        i.lines()
        .map(|l| l.parse().expect("number"))
        .collect();

    let mut n = 0usize;
    let mut p = 0isize;

    while p >= 0 && (p as usize) < v.len() {
        let d = v[p as usize];
        v[p as usize] += 1;
        p += d;
        n += 1;
    }

    n
}

fn solve_p2(i: &str) -> usize {
    let mut v: Vec<isize> =
        i.lines()
        .map(|l| l.parse().expect("number"))
        .collect();

    let mut n = 0usize;
    let mut p = 0isize;

    while p >= 0 && (p as usize) < v.len() {
        let d = v[p as usize];
        v[p as usize] += if d >= 3 { -1 } else { 1 };
        p += d;
        n += 1;
    }

    n
}

fn main() {
    let e = std::fs::read_to_string("example").expect("example");
    let i = std::fs::read_to_string("input").expect("input");

    println!("P1 example: {}", solve_p1(&e));
    println!("P1 ans: {}", solve_p1(&i));

    println!("P2 example: {}", solve_p2(&e));
    println!("P2 ans: {}", solve_p2(&i));
}
