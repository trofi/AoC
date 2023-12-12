use std::collections::HashMap;

fn parse_input(i: &str) -> Vec<(&str, Vec<usize>)> {
    i.lines().map(|l| {
        let iv: Vec<&str> = l.split(" ").collect();

        match iv.as_slice() {
            [m, vs] => (*m, vs.split(",").map(|e| e.parse().expect("number")).collect()),
            _ => panic!("Unexpected input {:?}", iv),
        }
    }).collect()
}

type Cache<'a> = HashMap<(&'a [char], &'a [usize]), usize>;


fn solve_one_cached<'a, 'b>(m: &'a [char], ns: &'a [usize], cache: &'b mut Cache<'a>) -> usize {
    if let Some(v) = cache.get(&(m, ns)) {
        return *v;
    }
    let r = solve_one(m, ns, cache);
    cache.insert((m, ns), r);
    return r;
}

fn solve_one<'a, 'b>(m: &'a [char], ns: &'a [usize], cache: &'b mut Cache<'a>) -> usize {
    // a few simple termination conditions:

    // last segment can only fit encoded problems
    if &[m.len()] == ns && m.iter().all(|c| *c == '#' || *c == '?') { return 1 }
    if ns.len() == 0 {
        if m.iter().all(|c| *c == '.' || *c == '?') { return 1 } else { return 0 }
    }

    // consumed all input
    if m.len() == 0 { return if ns.len() == 0 { 1 } else { 0 }}

    // input is shorter that template length
    if ns.len() > 0 && m.len() < ns.iter().sum::<usize>() + ns.len() - 1 { return 0 }

    let as_dot_prefix = if m[0] == '.' || m[0] == '?' {
        let r = solve_one_cached(&m[1..], ns, cache);
        r
    } else { 0 };

    let as_hash_prefix = if m[0] == '#' || m[0] == '?' {
        let n = ns[0];
        if m[1..n].iter().all(|c| *c == '#' || *c == '?') && (m[n] == '.' || m[n] == '?') {
            let r = solve_one_cached(&m[n+1..], &ns[1..], cache);
            r
        } else {
            0
        }
    } else { 0 };

    as_dot_prefix + as_hash_prefix
}

fn solve_p1(i: &str) -> usize {
    let m = parse_input(i);


    m.into_iter().map(|(m, ns)| {
        let cm: Vec<char> = m.chars().collect();

        let mut cache: Cache = Cache::new();

        let r = solve_one_cached(&cm, &ns, &mut cache);
        r
    }).sum()
}

fn solve_p2(i: &str) -> usize {
    let m = parse_input(i);


    m.into_iter().map(|(m, ns)| {
        let cm: Vec<char> = [m].repeat(5).join("?").chars().collect();
        let n: Vec<usize> = ns.repeat(5);

        let mut cache: Cache = Cache::new();

        let r = solve_one_cached(&cm, &n, &mut cache);
        r
    }).sum()
}

fn main() {
    let e = std::fs::read_to_string("example").expect("example");
    let i = std::fs::read_to_string("input").expect("input");
    println!("P1 example: {}", solve_p1(&e));
    println!("P1   input: {}", solve_p1(&i));
    println!("P2 example: {}", solve_p2(&e));
    println!("P2   input: {}", solve_p2(&i));
}
