fn solve_p1(step: usize) -> usize {
    let mut v: Vec<usize> = vec![0];
    let mut p: usize = 0;

    for i in 1..=2017 {
        p = (p + step) % v.len();
        v.insert(p + 1, i);
        p += 1;
    }

    v[(p + 1) % v.len()]
}

fn solve_p2(step: usize) -> usize {
    let mut l: usize = 1;
    let mut p: usize = 0;
    let mut v: Option<usize> = None;

    for i in 1..=50_000_000 {
        p = (p + step) % l;
        if p == 0 {
            v = Some(i);
        }
        l += 1;
        p += 1;
    }

    v.expect("value")
}

fn main() {
    println!("P1 example: {}", solve_p1(3));
    println!("P1 ans: {}", solve_p1(303));
    println!("P2 ans: {}", solve_p2(303));
}
