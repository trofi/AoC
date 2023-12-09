use std::iter::once;

fn parse_input(i: &str) -> Vec<Vec<isize>> {
    i.lines()
     .map(|l| {
         l.split(" ")
          .map(|e| e.parse().expect("number"))
          .collect()
    }).collect()
}

fn next_poly(p: &[isize]) -> Vec<isize> {
    p.iter()
     .chain(once(&0)).zip(
         once(&0).chain(p.iter()))
     .map(|(l,r)| l + r)
     .collect()
}

fn seq_to_cs(v: &[isize]) -> (Vec<isize>, Vec<isize>) {
    let l = v.len();
    let mut cs: Vec<isize> = Vec::new();
    let mut poly: Vec<isize> = vec![1];

    for ix in 0..l {
        let c: isize = v[ix] - poly.iter().zip(cs.iter()).map(|(l,r)| l*r).sum::<isize>();
        cs.push(c);
        poly = next_poly(&poly);
    }

    (cs, poly)
}

fn cs_to_last(v: &[isize], poly: &[isize]) -> isize {
    poly.iter()
        .zip(v.iter())
        .map(|(l,r)| l*r)
        .sum()
}

fn solve_p1(i: &str) -> isize {
    let vs = parse_input(i);
    vs.into_iter().map(|v| {
        let (mut cs, poly) = seq_to_cs(&v);
        let r = cs_to_last(&cs, &poly);
        cs.push(0);
        r
    }).sum()
}

fn cs_to_first(v: &[isize]) -> isize {
    v.iter().rev().fold(0, |acc, v| v - acc)
}

fn solve_p2(i: &str) -> isize {
    let vs = parse_input(i);
    vs.into_iter().map(|v| {
        let (cs, _) = seq_to_cs(&v);
        cs_to_first(&cs)
    }).sum()
}

fn main() {
    let e = std::fs::read_to_string("example").expect("example");
    let i = std::fs::read_to_string("input").expect("input");
    println!("P1 example: {}", solve_p1(&e));
    println!("P1   input: {}", solve_p1(&i));
    println!("P2 example: {}", solve_p2(&e));
    println!("P2   input: {}", solve_p2(&i));
}
