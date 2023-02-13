use std::collections::BTreeSet;

fn is_abba(i: &str) -> bool {
    i.chars()
     .collect::<Vec<_>>()
     .windows(4)
     .any(|w| w[0] == w[3] && w[1] == w[2] && w[0] != w[1])
}

fn is_tls(i: &str) -> bool {
    let mut e = false;
    let mut o = false;

    for (ix, v) in i.split(['[', ']']).enumerate() {
        if ix % 2 == 0 {
            e |= is_abba(v);
        } else {
            o |= is_abba(v);
        }
    }

    e && !o
}

fn solve_p1(i: &str) -> usize {
    i.lines()
     .filter(|l| is_tls(l) )
     .count()
}

fn get_xyx(i: &str, xy: bool) -> BTreeSet<(char, char)> {
    i.chars()
     .collect::<Vec<_>>()
     .windows(3)
     .filter(|w| w[0] == w[2] && w[0] != w[1])
     .map(|w| if xy { (w[0], w[1]) } else { (w[1], w[0]) })
     .collect()
}

fn is_ssl(i: &str) -> bool {
    let mut e: BTreeSet<(char, char)> = BTreeSet::new();
    let mut o: BTreeSet<(char, char)> = BTreeSet::new();

    for (ix, v) in i.split(['[', ']']).enumerate() {
        if ix % 2 == 0 {
            e = e.union(&get_xyx(v, true)).cloned().collect();
        } else {
            o = o.union(&get_xyx(v, false)).cloned().collect();
        }
    }

    e.intersection(&o).count() > 0
}

fn solve_p2(i: &str) -> usize {
    i.lines()
     .filter(|l| is_ssl(l) )
     .count()
}

fn main() {
    let e = std::fs::read_to_string("example").expect("example");
    let e2 = std::fs::read_to_string("example2").expect("example2");
    let i = std::fs::read_to_string("input").expect("input");
    println!("P1 example: {}", solve_p1(&e));
    println!("P1 ans: {}", solve_p1(&i));
    println!("P2 example: {}", solve_p2(&e2));
    println!("P2 ans: {}", solve_p2(&i));
}
