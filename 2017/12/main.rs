use std::collections::HashMap;
use std::collections::HashSet;

fn parse(i: &str) -> HashMap<usize, Vec<usize>> {
    i.lines().map(|l|
        match l.split_once(" <-> ") {
            Some((ks, vs)) => {
                let k = ks.parse().expect("key number");
                let v = vs.split(", ").map(|e| e.parse().expect("value number")).collect();
                (k, v)
            },
            _ => panic!("Failed to handle {:?}", l),
        }
    ).collect()
}

fn solve(i: &str) -> (usize, usize) {
    let g = parse(i);

    let mut ix2s: Vec<HashSet<usize>> = Vec::new();
    let mut n2ix: HashMap<usize, usize> = HashMap::new();

    for (ix, k) in g.keys().enumerate() {
        ix2s.push(HashSet::from([*k]));
        n2ix.insert(*k, ix);
    }

    for (k, vs) in &g {
        let ik = *n2ix.get(k).expect("k ix");
        for v in vs {
            let iv = *n2ix.get(v).expect("v ix");

            if ik != iv {
                let u = ix2s[ik].union(&ix2s[iv]).cloned().collect();
                ix2s[ik] = u;
                ix2s[iv] = HashSet::new();
                for rv in &ix2s[ik] {
                    n2ix.insert(*rv, ik);
                }
            }
        }
    }

    (ix2s[*n2ix.get(&0).expect("ix 0")].len(),
    ix2s.into_iter().filter(|s| !s.is_empty()).count())
}

fn main() {
    let e = std::fs::read_to_string("example").expect("example");
    let i = std::fs::read_to_string("input").expect("input");

    println!("P(1, 2) example: {:?}", solve(&e));
    println!("P(1, 2) ans: {:?}", solve(&i));
}
