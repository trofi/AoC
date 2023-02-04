use std::collections::BTreeSet;

fn solve(i: &str, groups: usize) -> usize {
    let ps: Vec<usize> =
        i.lines()
         .into_iter()
         .map(|l| l.parse()
                   .expect("number"))
         .collect();
    let s: usize = ps.iter().sum();
    assert!(s % groups == 0);
    let s = s / groups;

    let mut opts: BTreeSet<BTreeSet<usize>> = BTreeSet::new();
    opts.insert(BTreeSet::new());

    let mut min_c = usize::MAX;

    for p in &ps {
        let mut new_opts: BTreeSet<BTreeSet<usize>> = BTreeSet::new();
        for o in &opts {
            // micro-optimization to cut of early
            if o.iter().sum::<usize>() > s  { continue }
            if o.len() > min_c { continue }

            let mut new_o = o.clone();
            new_o.insert(*p);
            new_opts.insert(new_o.clone());

            if new_o.iter().sum::<usize>() == s && new_o.len() < min_c {
                min_c = o.len();
            }
        }
        opts = opts.union(&new_opts).cloned().collect();
    }
    opts.retain(|o| o.iter().sum::<usize>() == s);

    opts.into_iter()
        .map(|o| (o.len(), o.iter().product()))
        .min()
        .expect("at least one element").1
}

fn main() {
    let i = std::fs::read_to_string("input").expect("input");
    println!("P1 ans: {:?}", solve(&i, 3));
    println!("P2 ans: {:?}", solve(&i, 4));
}
