use std::collections::HashMap;

#[derive(Debug)]
struct Entry<'a> {
    name: &'a str,
    weight: usize,
    children: Vec<&'a str>,
}

fn parse<'a>(i: &'a str) -> Vec<Entry<'a>> {
    i.lines().map(|l|{
        let vw: Vec<_> =
            l.split(&[' ', '(', ')', ',', '-', '>'])
             .filter(|w| !w.is_empty())
             .collect();
        match vw.as_slice().split_at(2) {
            ([n, sw], cs) => Entry {
                name: n,
                weight: sw.parse().expect("unsignd weight"),
                children: Vec::from(cs),
            },
            _ => panic!("Unhandled {:?}", vw),
        }
    }).collect()
}

fn solve_p1<'a>(i: &'a str) -> &'a str {
    let p = parse(i);

    // child -> parent map
    let mut t: HashMap<&str, &str> = HashMap::new();

    for e in &p {
        for c in &e.children {
            t.insert(c, e.name);
        }
    }

    // pick first element, any would do
    let mut n: &str = p[0].name;

    // walk up the root tree
    while let Some(p) = t.get(n) {
        n = p;
    }

    return n
}

fn solve_p2(i: &str) -> usize {
    // Find root. Lazy, could avoid one parse.
    let root = solve_p1(i);

    let p = parse(i);

    // name -> entry map
    let mut t: HashMap<&str, &Entry> = HashMap::new();
    for e in &p {
        t.insert(e.name, e);
    }

    // name -> tree weight map
    let mut wt: HashMap<&str, usize> = HashMap::new();

    // queue to traverse the tree
    let mut q: Vec<&str> = vec![root];

    // build weight map
    while let Some(n) = q.pop() {
        let e = t.get(n).expect("entry");

        if e.children.iter().all(|c| wt.get(c).is_some()) {
            let w = e.weight + e.children.iter().map(|c| *wt.get(c).expect("cw")).sum::<usize>();
            wt.insert(n, w);
            continue
        }

        q.push(n);
        for c in &e.children {
            q.push(c);
        }
    }

    // among all the mismatching find the branch with smallest weight
    let mut r = (usize::MAX, usize::MAX);

    q.push(root);

    // find mismatches
    while let Some(n) = q.pop() {
        let e = *t.get(n).expect("entry");

        for c in &e.children {
            q.push(c);
        }

        let mut ws: Vec<usize> =
            e.children
             .iter()
             .map(|c| *wt.get(c).expect("cw"))
             .collect();

        if ws.is_empty() { continue }

        ws.sort();
        let f = ws.first().expect("first");
        let l = ws.last().expect("last");
        if l != f {
            // Assume we have at least 3 to pick the invalid node from
            // somewhere in the middle (not begginning and not the end).
            assert!(ws.len() >= 3);
            let expected_value = ws[1];

            // Fish out value of the imbalance.
            let unexpected_value =
                *ws.iter()
                  .filter(|e| **e != expected_value)
                  .next()
                  .expect("one unexpected");

            // Find problematic node (or it's ancestor)
            let uen =
                *e.children
                 .iter()
                 .filter(|c| *wt.get(*c).expect("cw") == unexpected_value)
                 .next()
                 .expect("entry for unexpected");
            let ue = t.get(uen).expect("u entry");

            // Disambiguate among the ancestors. Ideally should use
            // depth as a key, not the node values.
            let nv = (expected_value, expected_value - ue.children.iter().map(|c| *wt.get(c).expect("cw")).sum::<usize>());
            r = std::cmp::min(r, nv);
        }
    }

    r.1
}

fn main() {
    let e = std::fs::read_to_string("example").expect("example");
    let i = std::fs::read_to_string("input").expect("input");

    println!("P1 example: {}", solve_p1(&e));
    println!("P1 ans: {}", solve_p1(&i));

    println!("P2 example: {}", solve_p2(&e));
    println!("P2 ans: {}", solve_p2(&i));
}
