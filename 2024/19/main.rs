use std::collections::HashMap;

type Cache<'a> = HashMap<&'a str, usize>;

fn parse_input(i: &str) -> (Vec<&str>, Vec<&str>) {
    let (ks, ps) = i.split_once("\n\n").expect("double newline as a separator");

    let kv: Vec<&str> = ks.split(", ").collect();
    let pv: Vec<&str> = ps.lines().collect();

    (kv, pv)
}

fn arrangements<'a>(p: &'a str, kv: &[&'a str], cache: &mut Cache<'a>) -> usize {
    if let Some(v) = cache.get(&p) { return *v; }

    let mut r: usize = 0;

    for k in kv {
        if !p.starts_with(k) { continue }
        if p == *k { r += 1; continue; }

        let ptail = &p[k.len() ..];
        r += arrangements(ptail, kv, cache);
    }

    cache.insert(p, r);

    r
}

fn solve_p1(i: &str) -> usize {
    let (kv, pv) = parse_input(i);

    let mut cache = Cache::new();
    pv.into_iter().filter(|p| arrangements(p, &kv, &mut cache) > 0).count()
}

fn solve_p2(i: &str) -> usize {
    let (kv, pv) = parse_input(i);

    let mut cache = Cache::new();
    pv.into_iter().map(|p| arrangements(p, &kv, &mut cache)).sum()
}

fn main() {
    let e = std::fs::read_to_string("example").expect("example");
    let i = std::fs::read_to_string("input").expect("input");
    println!("P1 e: {:?}", solve_p1(&e));
    println!("P1 i: {:?}", solve_p1(&i));
    println!("P2 e: {:?}", solve_p2(&e));
    println!("P2 i: {:?}", solve_p2(&i));
}
