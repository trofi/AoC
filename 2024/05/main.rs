use std::collections::HashSet;

type Rule = (usize, usize);
type Update = Vec<usize>;

fn parse_input(i: &str) -> (Vec<Rule>, Vec<Update>) {
    let (rules_s, updates_s) = i
        .split_once("\n\n")
        .expect("rules and updates");

    let rules: Vec<Rule> = rules_s
        .lines()
        .map(|l| {
            let v: Vec<usize> = l
                .split('|')
                .map(|e| e.parse::<usize>().expect("a number"))
                .collect();
            match v.as_slice() {
                [a, b] => (*a, *b),
                _ => panic!("Unhandled '{:?}'", l),
            }
        }).collect();

    let updates: Vec<Update> = updates_s
        .lines()
        .map(|l|
            l.split(',')
             .map(|e| e.parse::<usize>().expect("a number"))
             .collect()
        ).collect();

    (rules, updates)
}

fn solve_p1(i: &str) -> usize {
    let (rules, updates) = parse_input(i);

    let mut ordered: HashSet<(usize, usize)> = HashSet::new();

    for r in &rules {
        ordered.insert(*r);
    }

    let mut r = 0;

    for u in &updates {
        let len = u.len();

        let mut ok: bool = true;
        // brote force pairs to check if none of them are violating
        'scan: for l in 0..len {
            for r in (l+1)..len {
                // must never violate
                if ordered.contains(&(u[r], u[l])) {
                    ok = false;
                    break 'scan;
                }
            }
        }

        if !ok { continue }

        r += u[len / 2];
    }

    r
}

fn solve_p2(i: &str) -> usize {
    let (rules, mut updates) = parse_input(i);

    let mut ordered: HashSet<(usize, usize)> = HashSet::new();

    for r in &rules {
        ordered.insert(*r);
    }

    let mut r = 0;

    for u in &mut updates {
        let len = u.len();

        let mut ok: bool = true;

        // Brote force pairs and reorder violators.
        // It should be an equivalent of naive bubblesort.
        for l in 0..len {
            for r in (l+1)..len {
                // Violated constraints: reorder.
                if ordered.contains(&(u[r], u[l])) {
                    ok = false;
                    // swap:
                    let t: usize = u[r];
                    u[r] = u[l];
                    u[l] = t;
                }
            }
        }

        if !ok {
            r += u[len / 2];
        }
    }

    r
}

fn main() {
    let e = std::fs::read_to_string("example").expect("example");
    let i = std::fs::read_to_string("input").expect("input");
    println!("P1 e: {:?}", solve_p1(&e));
    println!("P1 i: {:?}", solve_p1(&i));
    println!("P2 e: {:?}", solve_p2(&e));
    println!("P2 i: {:?}", solve_p2(&i));
}
