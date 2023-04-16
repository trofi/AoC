fn parse(i: &str) -> Vec<(usize, usize)> {
    i.lines().map(|l| {
        let v: Vec<_> = l.split(&[' ', ':']).collect();
        match v.as_slice() {
            [ns, "", rs] => (ns.parse().expect("layer"), rs.parse().expect("size")),
            _ => panic!("Unknonw {:?} input", v),
        }
    }).collect()
}

fn solve_p1(i: &str) -> usize {
    let f = parse(i);

    f.into_iter().map(|(n, r)| {
        // Example: len 4
        // 0 | 0     6       12
        // 1 |  1   5 7    11
        // 2 |   2 4   8 10
        // 3 |    3     9
        let o = n % (2 * (r - 1));
        let ow = if o < r { o } else { 2 * (r - 1) - o };

        if ow == 0 { n * r } else { 0 }
    }).sum()
}

fn solve_p2(i: &str) -> usize {
    let f = parse(i);

    for d in 0.. {

        let r: usize = f.iter().map(|(n, r)| {
            // Example: len 4
            // 0 | 0     6       12
            // 1 |  1   5 7    11
            // 2 |   2 4   8 10
            // 3 |    3     9
            let o = (*n + d) % (2 * (*r - 1));
            let ow = if o < *r { o } else { 2 * (*r - 1) - o };

            if ow == 0 { 1 } else { 0 }
        }).sum();
        if r == 0 { return d }
    }
    panic!("No soultion?");
}

fn main() {
    let e = std::fs::read_to_string("example").expect("example");
    let i = std::fs::read_to_string("input").expect("input");

    println!("P1 example: {}", solve_p1(&e));
    println!("P1 ans: {}", solve_p1(&i));
    println!("P2 example: {}", solve_p2(&e));
    println!("P2 ans: {}", solve_p2(&i));
}
