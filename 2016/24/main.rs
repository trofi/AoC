use std::collections::BinaryHeap;
use std::collections::BTreeMap;
use std::collections::BTreeSet;

type Pos = (isize, isize);
type Map = BTreeMap<Pos, char>;

fn parse(i: &str) -> Map {
    let mut m = Map::new();

    for (y, l) in i.lines().enumerate() {
        for (x, c) in l.chars().enumerate() {
            if c == '#' { continue }

            m.insert((x as isize, y as isize), c);
        }
    }

    m
}

fn find_path(m: &Map, from: Pos, to: Pos) -> usize {
    #[derive(Eq, Ord, PartialEq, PartialOrd)]
    struct Key {
        steps: usize,
        p: Pos,
    }
    let with_prio = |k: Key| -> (isize, Key) {
        let prio = k.steps as isize + (k.p.0 - to.0).abs() + (k.p.1 - to.1).abs();

        (-prio, k)
    };

    let mut q: BinaryHeap<(isize, Key)> = BinaryHeap::new();

    q.push(with_prio(Key{
        steps: 0,
        p: from,
    }));

    let mut visited: BTreeSet<Pos> = BTreeSet::new();

    while let Some((_, k)) = q.pop() {
        if visited.contains(&k.p) { continue }
        visited.insert(k.p);

        if k.p == to { return k.steps }

        let steps = k.steps + 1;
        for (dx, dy) in [(-1, 0), (1, 0), (0, -1), (0, 1)] {
            let p = (k.p.0 + dx, k.p.1 + dy);
            if m.contains_key(&p) {
                q.push(with_prio(Key{
                    steps,
                    p,
                }));
            }
        }
    }

    todo!()
}

fn solve(i: &str, and_return: bool) -> usize {
    let m = parse(i);
    let goals: BTreeMap<char, Pos> =
        m.iter()
         .filter(|(_, v)| v.is_digit(10))
         .map(|(k, v)| (*v, *k))
         .collect();
    let start = *goals.get(&'0').expect("start pos");
    println!("Goals: {:?}; start={:?}", goals, start);

    #[derive(Eq, Ord, PartialEq, PartialOrd)]
    struct Key {
        p: Pos,
        visited: BTreeSet<char>,
        steps: usize,
    }
    fn with_prio(k: Key) -> (isize, Key) {
        let prio = k.steps as isize;

        // Better priority would be the distance to furtherst
        // unvisited node.
        (- prio, k)
    }

    let mut q: BinaryHeap<(isize, Key)> = BinaryHeap::new();
    q.push(with_prio(Key{
        p: start,
        visited: BTreeSet::new(),
        steps: 0,
    }));

    let mut r = usize::MAX;

    while let Some((_, k)) = q.pop() {
        let res = k.steps + if and_return {
            find_path(&m, k.p, start)
        } else {
            0
        };

        if res > r { continue }

        if k.visited.len() == goals.len() {
            if res < r {
                println!("Improved solution: {} -> {}", r, res);
                r = res;
            }
        }

        for g in &goals {
            if k.visited.contains(&g.0) { continue }
            let steps = k.steps + find_path(&m, k.p, *g.1);
            let mut visited = k.visited.clone();
            visited.insert(*g.0);

            q.push(with_prio(Key{
                p: *g.1,
                visited,
                steps,
            }));
        }
    }

    r
}

fn main() {
    let e = std::fs::read_to_string("example").expect("example");
    let i = std::fs::read_to_string("input").expect("input");
    println!("P1 example: {}", solve(&e, false));
    println!("P1 ans: {}", solve(&i, false));
    println!("P2 ans: {}", solve(&i, true));
}
