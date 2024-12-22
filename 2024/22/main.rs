use std::collections::HashMap;
use std::collections::HashSet;

fn parse_input(i: &str) -> Vec<usize> {
    i.lines()
     .map(|l| l.parse::<usize>().expect("a number"))
     .collect()
}

fn mix(v: usize, n: usize) -> usize { v ^ n }

fn prune(v: usize) -> usize { v % 16777216 }

fn step(mut n: usize) -> usize {
    n = mix(n * 64, n);
    n = prune(n);
    n = mix(n / 32, n);
    n = prune(n);
    n = mix(n * 2048, n);
    n = prune(n);

    n
}

fn generate_seq(mut n: usize, l: usize) -> Vec<usize> {
   let mut r = Vec::new();
   r.push(n);
    for _ in 0..l {
        n = step(n);
        r.push(n);
    }
    r
}

fn solve_p1(i: &str) -> usize {
    let nv = parse_input(i);

    nv.into_iter()
      .map(|e| generate_seq(e, 2001)[2000])
      .sum()
}

fn price_changes(n: usize) -> Vec<(usize, isize)> {
    let mut res = Vec::new();

    let ps: Vec<usize> = generate_seq(n, 2001)
        .into_iter()
        .map(|e| e % 10)
        .collect();

    for (l, r) in ps.iter().zip(ps.iter().skip(1)) {
        let delta: isize = (*r as isize) - (*l as isize);
        res.push((*r, delta));
    }

    res
}

fn solve_p2(i: &str) -> usize {
    let nv = parse_input(i);

    let mut hist_price_sum: HashMap<Vec<isize>, usize> = HashMap::new();

    for n in nv.into_iter() {
        let mut seen_diffs: HashSet<Vec<isize>> = HashSet::new();

        for w in price_changes(n).windows(4) {
            let dv: Vec<isize> = w.iter().map(|e| e.1).collect();

            if seen_diffs.contains(&dv) { continue }
            seen_diffs.insert(dv.clone());

            let price: usize = w.last().expect("a value in the window").0;

            hist_price_sum
                .entry(dv.clone())
                .and_modify(|e| *e += price)
                .or_insert(price);
        }
    }

    *hist_price_sum.values().max().expect("at least one entry")
}

fn main() {
    let e = std::fs::read_to_string("example").expect("example");
    let e2 = std::fs::read_to_string("example2").expect("example2");
    let i = std::fs::read_to_string("input").expect("input");

    println!("P1  e: {:?} (expect 37327623)", solve_p1(&e));
    println!("P1  i: {:?}", solve_p1(&i));
    println!("P2 e2: {:?}", solve_p2(&e2));
    println!("P2  i: {:?}", solve_p2(&i));
}
