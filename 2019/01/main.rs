fn solve_p1(i: &str) -> usize {
    i.lines().map(|e| {
        let v = e.parse::<usize>().expect("a number");

        v / 3 - 2
    }).sum()
}

fn add_p2(mut v: usize) -> usize {
    let mut r = v;
    loop {
        if v < 6 { break }

        v = v / 3 - 2;
        r += v;
    }

    r
}

fn solve_p2(i: &str) -> usize {
    i.lines().map(|e| {
        add_p2(solve_p1(e))
    }).sum()
}


fn main() {
    let e = std::fs::read_to_string("example").expect("example");
    let i = std::fs::read_to_string("input").expect("input");
    println!("P1 e: {:?}", solve_p1(&e));
    println!("P1 i: {:?}", solve_p1(&i));
    println!("P2 e: {:?}", solve_p2(&e));
    println!("P2 i: {:?}", solve_p2(&i));
}
