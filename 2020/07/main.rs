use std::collections::HashMap;
use std::collections::HashSet;

type Color<'a> = (&'a str, &'a str);

fn parse_input<'a>(i: &'a str) -> HashMap<Color<'a>, Vec<(usize, Color<'a>)>> {
    i.lines().map(|l| {
        // example: "dark orange bags contain 3 bright white bags, 4 muted yellow bags."
        //to: ["dark", "orange", "bags", "contain", "3", "bright", "white", "bags", "", "4", "muted", "yellow", "bags", ""]
        let vl: Vec<&str> = l.split(&[' ', ',', '.']).collect();
        let (l, r) = vl.split_at(4);
        let k = (l[0], l[1]);
        assert!(l[2] == "bags");
        assert!(l[3] == "contain");

        let v = r.chunks(5).filter(|e| e != &["no", "other", "bags", ""]).map(|e| {
            let n: usize = e[0].parse().expect("a number");
            let vc: Color = (e[1], e[2]);
            assert!(e[3] == "bags" || e[3] == "bag");
            assert!(e[4] == "");

            (n, vc)
        }).collect();

        (k, v)
    }).collect()
}

fn solve_p1(i: &str) -> usize {
    let p = parse_input(i);

    let mut holder: HashMap<Color, Vec<Color>> = HashMap::new();

    // color reverse mapping (adjacency list)
    for (kc, val) in p.into_iter() {
        for (_, vc) in val.into_iter() {
            holder.entry(vc)
                  .and_modify(|v| v.push(kc))
                  .or_insert(vec![kc]);
        }
    }

    // DFS
    let mut seen: HashSet<Color> = HashSet::new();
    let mut q: Vec<Color> = vec![("shiny", "gold")];

    while let Some(c) = q.pop() {
        if seen.contains(&c) { continue }
        seen.insert(c);

        if let Some(c_holders) = holder.get(&c) {
            for nc in c_holders {
                q.push(*nc);
            }
        }
    }

    seen.len() - 1
}

fn solve_p2(i: &str) -> usize {
    let p = parse_input(i);

    // DP: cache values for each key
    let mut cache: HashMap<Color, usize> = HashMap::new();

    fn get_value<'a>(c: &Color<'a>, p: &HashMap<Color<'a>, Vec<(usize, Color<'a>)>>, cache: &mut HashMap<Color<'a>, usize>) -> usize {
        if let Some(v) = cache.get(c) { return *v }

        let v = p.get(c).expect("known color");
        let mut r = 1;
        for (n, nc) in v {
            r += n * get_value(nc, p, cache);
        }

        cache.insert(*c, r);
        r
    }

    get_value(&("shiny", "gold"), &p, &mut cache) - 1
}

fn main() {
    let e = std::fs::read_to_string("example").expect("example");
    let e2 = std::fs::read_to_string("example2").expect("example2");
    let i = std::fs::read_to_string("input").expect("input");
    println!("P1 e: {:?}", solve_p1(&e));
    println!("P1 i: {:?}", solve_p1(&i));
    println!("P2 e: {:?}", solve_p2(&e2));
    println!("P2 i: {:?}", solve_p2(&i));
}
