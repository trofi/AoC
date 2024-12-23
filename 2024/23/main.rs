use std::collections::HashMap;
use std::collections::HashSet;
use std::iter::FromIterator;

fn parse_input(i: &str) -> Vec<(&str, &str)> {
    i.lines()
     .map(|l| l.split_once('-').expect("a pair"))
     .collect()
}

fn solve_p1(i: &str) -> usize {
    let pi = parse_input(i);

    let mut direct_link: HashSet<(&str, &str)> = HashSet::new();
    for (l, r) in &pi {
        direct_link.insert((*l, *r));
        direct_link.insert((*r, *l));
    }

    let mut set3s: HashSet<[&str; 3]> = HashSet::new();
    for (a, b) in &pi {
        for (c, d) in &pi {
            if c == a && direct_link.contains(&(*a, *d)) && direct_link.contains(&(*b, *d)) {
                let mut s = [*a, *b, *d];
                s.sort();
                set3s.insert(s);
            }
            if d == a && direct_link.contains(&(*a, *c)) && direct_link.contains(&(*b, *c)) {
                let mut s = [*a, *b, *c];
                s.sort();
                set3s.insert(s);
            }
        }
    }

    set3s.into_iter()
       .filter(|s| s.len() == 3 && s.iter().any(|e| e.starts_with("t")))
       .count()
}

fn solve_p2(i: &str) -> String {
    let pi = parse_input(i);

    let mut direct_link: HashSet<(&str, &str)> = HashSet::new();
    for (l, r) in &pi {
        direct_link.insert((*l, *r));
        direct_link.insert((*r, *l));
    }

    // collect largest all-connected sets
    let mut dc_map: HashMap<&str, HashSet<&str>> = HashMap::new();
    for (l, r) in &pi {
        dc_map
            .entry(l)
            .and_modify(|s| {
                if s.iter().all(|e| direct_link.contains(&(*e, *r))) {
                    s.insert(*r);
                }
            })
        .or_insert(HashSet::from([*l, *r]));
        dc_map
            .entry(r)
            .and_modify(|s| {
                if s.iter().all(|e| direct_link.contains(&(*e, *l))) {
                    s.insert(*l);
                }
            })
        .or_insert(HashSet::from([*l, *r]));
    }

    // sort by set size, build the string from largest one
    dc_map
        .values()
        .map(|s| {
            let mut v: Vec<&str> = Vec::from_iter(s.clone());
            v.sort();
            (v.len(), v)
        })
        .max()
        .expect("at least one element").1.join(",")
}

fn main() {
    let e = std::fs::read_to_string("example").expect("example");
    let i = std::fs::read_to_string("input").expect("input");
    println!("P1 e: {:?} (expect 7)", solve_p1(&e));
    println!("P1 i: {:?}",            solve_p1(&i));
    println!("P1 e: {:?} (expect \"co,de,ka,ta\")", solve_p2(&e));
    println!("P1 i: {:?}",                      solve_p2(&i));
}
