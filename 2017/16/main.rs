use std::iter::FromIterator;
use std::collections::HashMap;

fn solve_p1(s: &str, i: &str) -> String {
    let mut v: Vec<char> = Vec::from_iter(s.chars());

    for cmd in i.split(",") {
        match cmd.split_at(1) {
            ("s", ns) => {
                let n: usize = ns.parse().expect("number");
                v = (0..v.len()).map(|i| v[(i + v.len() - n) % v.len()])
                                .collect();
            },
            ("x", ab) => {
                let abv: Vec<&str> = ab.split("/").collect();
                match abv.as_slice() {
                    [is, js] => {
                        let i: usize = is.parse().expect("number");
                        let j: usize = js.parse().expect("number");

                        let e = v[i];
                        v[i] = v[j];
                        v[j] = e;
                    },
                    _ => panic!("Unhandled {:?}", abv),
                }
            },
            ("p", ab) => {
                let abv: Vec<&str> = ab.split("/").collect();
                match abv.as_slice() {
                    [is, js] => {
                        let ic = is.chars().next().expect("c");
                        let jc = js.chars().next().expect("c");
                        let i: usize = v.iter().position(|e| *e == ic).expect("number");
                        let j: usize = v.iter().position(|e| *e == jc).expect("number");

                        let e = v[i];
                        v[i] = v[j];
                        v[j] = e;
                    },
                    _ => panic!("Unhandled {:?}", abv),
                }
            },
            cv => panic!("Unknown cmd {:?}", cv),
        }
    }

    String::from_iter(&v)
}

fn solve_p2(s: &str, prog: &str) -> String {
    let mut seen: HashMap<String, usize> = HashMap::new();
    let mut ns = String::from(s);

    for i in 0.. {
        if let Some(ix) = seen.get(&ns) {
            let loop_len = i - ix;
            let left = (1_000_000_000 - ix) % loop_len;
            for _ in 0..left {
                ns = solve_p1(&ns, prog);
            }
            return ns;
        }
        seen.insert(ns.clone(), i);
        ns = solve_p1(&ns, prog);
    }

    ns
}

fn main() {
    let e = "s1,x3/4,pe/b";
    let i = std::fs::read_to_string("input").expect("input");
    println!("P1 example: {:?}", solve_p1("abcde", &e));
    println!("P1 ans: {:?}", solve_p1(&String::from_iter('a'..='p'), &i));
    println!("P2 example: {:?}", solve_p2("abcde", &e));
    println!("P2 ans: {:?}", solve_p2(&String::from_iter('a'..='p'), &i));
}
