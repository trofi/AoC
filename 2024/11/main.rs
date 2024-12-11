use std::collections::HashMap;

fn parse_input(i: &str) -> Vec<usize> {
    i.split(' ')
     .map(|e| e.parse::<usize>().expect("a number (input)" ))
     .collect()
}

fn split_number(n: usize) -> Vec<usize> {
    if n == 0 {
        return vec![1];
    } else {
        let ns = format!("{n}");
        let nl = ns.len();

        if nl % 2 == 0 {
            let (lns, rns) = ns.split_at(nl / 2);
            let ln = lns.parse::<usize>().expect("a number (left)");
            let rn = rns.parse::<usize>().expect("a number (right)");
            return vec![ln, rn];
        } else {
            return vec![n * 2024];
        }
    }
}

fn get_depth(n: usize, depth: usize, mut mem: &mut HashMap<(usize, usize), usize>) -> usize {
    // bottom: just one number
    if depth == 0 { return 1 }

    // lookup cache
    if let Some(r) = mem.get(&(n, depth)) { return *r }

    let r = split_number(n)
        .into_iter()
        .map(|sn| get_depth(sn, depth -1, &mut mem))
        .sum();

    // cache
    mem.insert((n, depth), r);

    r
}

fn solve(i: &str, depth: usize) -> usize {
    let l = parse_input(i);

    // (number, depth) => value
    let mut mem: HashMap<(usize, usize), usize> = HashMap::new();

    l.into_iter()
     .map(|e| get_depth(e, depth, &mut mem))
     .sum()
}

fn main() {
    let e = std::fs::read_to_string("example").expect("example");
    let i = std::fs::read_to_string("input").expect("input");
    println!("P1 e: {:?}", solve(&e, 25));
    println!("P1 i: {:?}", solve(&i, 25));
    println!("P2 e: {:?}", solve(&e, 75));
    println!("P2 i: {:?}", solve(&i, 75));
}
