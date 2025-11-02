#[derive(Debug)]
struct Entry<'a> {
    c: char,
    range: (usize, usize),
    pass: &'a str,
}

fn parse_entry<'a>(i: &'a str) -> Entry<'a> {
    // example: "1-3 a: abcde"
    match i.split_once(": ") {
        Some((spec, pass)) => {
            let vs: Vec<&str> = spec.split(['-', ' ']).collect();
            match vs.as_slice() {
                &[s_from, s_to, c] => Entry {
                    c: c.chars().next().expect("a char"),
                    range: (s_from.parse().expect("from"), s_to.parse().expect("to")),
                    pass: pass,
                },
                _ => panic!("Unexpected spec={:?} / vs={:?}", spec, vs),
            }
        },
        _ => panic!("Unexpected input={:?}", i),
    }
}

fn parse_input<'a>(i: &'a str) -> Vec<Entry<'a>> {
    i.lines().map(|l| parse_entry(l)).collect()
}

fn solve_p1(i: &str) -> usize {
    let v = parse_input(i);
    v.iter().filter(|e| {
        let cn = e.pass.chars().filter(|c| *c == e.c).count();

        e.range.0 <= cn && cn <= e.range.1
    }).count()
}

fn solve_p2(i: &str) -> usize {
    let v = parse_input(i);
    v.iter().filter(|e| {
        let vc: Vec<char> = e.pass.chars().collect();
        let mut svc: Vec<char> = vec![];
        for ix in [e.range.0 - 1, e.range.1 - 1] {
            if ix < vc.len() {
                svc.push(vc[ix]);
            }
        }

        let cn = svc.into_iter().filter(|c| *c == e.c).count();

        cn == 1
    }).count()
}

fn main() {
    let e = std::fs::read_to_string("example").expect("example");
    let i = std::fs::read_to_string("input").expect("input");
    println!("P1 e: {:?}", solve_p1(&e));
    println!("P1 i: {:?}", solve_p1(&i));
    println!("P2 e: {:?}", solve_p2(&e));
    println!("P2 i: {:?}", solve_p2(&i));
}
