use std::collections::HashMap;

fn solve(i: &str) -> (usize, usize) {
    let mut bs: Vec<usize> =
        i.trim()
         .split("\t")
         .map(|e| e.parse().expect("number"))
         .collect();
    let bl = bs.len();

    let mut s: HashMap<_, _> = HashMap::new();

    for i in 0.. {
        if let Some(ix) = s.get(&bs) { return (i, i - ix) }
        s.insert(bs.clone(), i);

        let max_b = *bs.iter().max().expect("largest");
        let max_b_ix = bs.iter().position(|e| *e == max_b).expect("largest");
        let q = max_b / bl;
        let r = max_b % bl;

        bs[max_b_ix] = 0;

        for ix in 0..bl {
            bs[(max_b_ix + 1 + ix) % bl] += q + if ix < r { 1 } else { 0 }
        }
    }
    panic!("Looped indefinitely!")
}

fn main() {
    let e = std::fs::read_to_string("example").expect("example");
    let i = std::fs::read_to_string("input").expect("input");

    println!("P(1, 2) example: {:?}", solve(&e));
    println!("P(1, 2) ans: {:?}", solve(&i));
}
