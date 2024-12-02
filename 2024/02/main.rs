fn parse_input(i: &str) -> Vec<Vec<isize>> {
    i.lines().map(|l|
        l.split(" ").map(|e|
            e.parse::<isize>().expect("a number")
        ).collect()
    ).collect()
}

fn is_safe(v: &[isize]) -> bool {
    let i = std::iter::zip(v, v.iter().skip(1));
    let is_desc = (i.clone()).all(|(l, r)| {
        let d = l - r;
        d >= -3 && d <= -1
    });
    let is_asc = (i.clone()).all(|(l, r)| {
        let d = l - r;
        d >= 1 && d <= 3
    });
    is_desc || is_asc
}

fn solve_p1(i: &str) -> usize {
    let iv = parse_input(i);

    iv.into_iter()
      .filter(|e| is_safe(e))
      .count()
}

fn skip_one(v: &[isize]) -> Vec<Vec<isize>> {
    (0..v.len())
        .map(|i| {
            let mut vc: Vec<isize> = Vec::from(v);
            vc.remove(i);
            vc
        }).collect()
}

fn solve_p2(i: &str) -> usize {
    let iv = parse_input(i);

    iv.into_iter()
      .filter(|e|
          is_safe(e) || skip_one(e).into_iter().any(|v| is_safe(&v))
      ).count()
}

fn main() {
    let e = std::fs::read_to_string("example").expect("example");
    let i = std::fs::read_to_string("input").expect("input");
    println!("P1 e: {:?}", solve_p1(&e));
    println!("P1 i: {:?}", solve_p1(&i));
    println!("P2 e: {:?}", solve_p2(&e));
    println!("P2 i: {:?}", solve_p2(&i));
}
