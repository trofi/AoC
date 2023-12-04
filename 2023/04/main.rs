use std::collections::HashSet;
use std::iter::FromIterator;

struct Card {
    winning: Vec<usize>,
    have: Vec<usize>,
}

fn parse_card(i: &str) -> Card {
    // Example input: "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53"
    let iv: Vec<&str> = i.split([':', ' ']).filter(|l| l != &"").clone().collect();
    assert!(iv.len() >= 5);
    assert!(iv[0] == "Card");

    let nv: Vec<&[&str]> = iv[2..].split(|e| e == &"|").collect();
    match nv.as_slice() {
        [w, h] => Card{
            winning: w.iter().map(|e| e.parse().expect("number")).collect(),
            have: h.iter().map(|e| e.parse().expect("number")).collect(),
        },
        _ => panic!("Unhandled {:?} {:?} input", i, nv),
    }
}

fn parse_input(i: &str) -> Vec<Card> {
    i.lines().map(|l| parse_card(l)).collect()
}

fn solve_p1(i: &str) -> usize {
    let cs = parse_input(i);

    cs.into_iter().map(|g|{
        let w: HashSet<usize> = HashSet::from_iter(g.winning);
        let h: HashSet<usize> = HashSet::from_iter(g.have);
        let matches = h.intersection(&w).count();
        if matches > 0 { 2usize.pow(matches as u32 - 1) } else { 0 }
    }).sum()
}

fn solve_p2(i: &str) -> usize {
    let cs = parse_input(i);
    let l = cs.len();

    let mut v: Vec<usize> = Vec::new();
    v.resize(cs.len(), 1);

    for (i, c) in cs.into_iter().enumerate() {
        let w: HashSet<usize> = HashSet::from_iter(c.winning);
        let h: HashSet<usize> = HashSet::from_iter(c.have);
        let matches = h.intersection(&w).count();

        for j in (i+1)..(i+1+matches) {
            if j >= l { break }
            v[j] += v[i];
        }
    }

    v.into_iter().sum()
}

fn main() {
    let e = std::fs::read_to_string("example").expect("example");
    let i = std::fs::read_to_string("input").expect("input");
    println!("P1 example: {}", solve_p1(&e));
    println!("P1   input: {}", solve_p1(&i));
    println!("P2 example: {}", solve_p2(&e));
    println!("P2   input: {}", solve_p2(&i));
}
