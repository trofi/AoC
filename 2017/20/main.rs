type C3 = Vec<isize>; // [x, y, z]
type V3 = Vec<C3>;    // [p, v, a]

fn sig3(p3: &C3) -> C3 {
    p3.iter().map(|e| e.signum()).collect()
}

fn mul3(l3: &C3, r3: &C3) -> C3 {
    (0..3).map(|i| l3[i] * r3[i]).collect()
}

fn norm(v: &V3) -> V3 {
    let s3 = sig3(&v[2]);
    v.iter().map(|e| mul3(e, &s3)).collect()
}

fn parse_c3(i: &str) -> C3 {
    i.split(",")
     .map(|e| e.parse().expect("a number"))
     .collect()
}

fn parse_p(i: &str) -> V3 {
    // Assume something like:
    // "p=< 3,0,0>, v=< 2,0,0>, a=<-1,0,0>"
    i.split(&['<', '>'])
     .filter(|l| !l.contains("=") && !l.is_empty())
     .map(|l| parse_c3(l.trim()))
     .collect()
}

fn solve_p1(i: &str) -> usize {
    let mut ps: Vec<(usize, V3)> = Vec::new();

    for (ix, l) in i.lines().enumerate() {
        let p = parse_p(l);
        let pn = norm(&p);
        ps.push((ix, pn));
    }

    let mut rps: Vec<((isize, isize, isize), usize)> =
        ps.into_iter()
          .map(|(ix, v)| ((v[2].iter().sum(), v[1].iter().sum(), v[0].iter().sum()), ix))
          .collect();

    rps.sort();
    rps[0].1
}

fn solve_p2(_i: &str) -> usize {
    todo!("P2 solution")
}

fn main() {
    let e = std::fs::read_to_string("example").expect("example");
    let i = std::fs::read_to_string("input").expect("input");
    println!("P1 example: {}", solve_p1(&e));
    println!("P1 input: {}", solve_p1(&i));
    println!("P2 input: {}", solve_p2(&i));
}
