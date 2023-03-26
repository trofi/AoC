use std::collections::BTreeMap;
use std::collections::BTreeSet;
use std::collections::BinaryHeap;

type Pos = (isize, isize);

#[derive(Debug, Eq, Ord, PartialEq, PartialOrd, Clone, Copy)]
struct Node {
    used: isize,
    avail: isize,
}

fn parse_df(i: &str) -> BTreeMap<Pos, Node> {
    i.lines().skip(2).map(|l| {
        let ll = l.split_whitespace().collect::<Vec<_>>().join(" ");
        let lv: Vec<_> = ll.split(&['-', 'x', 'y', ' ', 'T']).collect();
        match lv.as_slice() {
            ["/dev/grid/node", "", sx, "", sy, _ssz, "", su, "", sa, "", _spfree] =>
                ((sx.parse().expect("number"), sy.parse().expect("number")),
                 Node{
                     used: su.parse().expect("number"),
                     avail: sa.parse().expect("number"),
                 }),
            _ => panic!("Unhandled {:?}", lv)
        }
    }).collect()
}

fn solve_p1(i: &str) -> usize {
    let m = parse_df(i);
    let mut r = 0;

    for (pn, n) in &m {
        for (pnn, nn) in &m {
            if pn != pnn && n.used > 0 && nn.avail >= n.used {
                r += 1;
            }
        }
    }

    r
}

// It's an A* search with many hacks and simplifications. A few goals:
// - assume that empty node can be moved around freely (except the
//   storage nodes)
// - move empty node "in front of" needed node
// - avoid a wall of storage nodes by measuting the path around them
fn solve_p2(i: &str) -> isize {
    let m = parse_df(i);

    let target =
        *m.keys()
          .filter(|(_, y)| *y == 0)
          .max().expect("max node");

    let mm: BTreeMap<Pos, (bool, Node)> =
        m.into_iter()
         .map(|(p, n)| (p, (p == target, n))).collect();

    #[derive(Debug, Eq, Ord, PartialEq, PartialOrd, Clone)]
    struct Key {
        steps: isize,
        m: BTreeMap<Pos, (bool, Node)>,
    }

    #[derive(Eq, Ord, PartialEq, PartialOrd)]
    struct PrioKey {
        dist: isize,
        empty_dist: isize,
    }

    fn with_prio(k: Key) -> (PrioKey, Key) {
        let (apos, _) = k.m.iter().filter(|(_, v)| v.0).next().expect("pos");
        let (epos, _) = k.m.iter().filter(|(_, v)| v.1.used == 0).next().expect("pos");
        let adist = apos.0 + apos.1;
        let edist =
            (apos.0 - 1 - epos.0).abs() +
            (apos.1 - epos.1).abs();

        // Heuristics hack: get past storage nodes wall.
        // Ideally we should use here a heuristic that
        // pushes us closer to an A* search path on otherwise
        // empty field.
        let corner_dist = if epos.0 > 19  && epos.1 > 23 {
            (epos.0 - 19) + (epos.1 - 23)
        } else {
            0
        };

        let pk = PrioKey {
            dist: -adist,
            empty_dist: -(k.steps + edist + corner_dist),
        };
        (pk, k)
    }

    let mut q: BinaryHeap<(PrioKey, Key)> = BinaryHeap::new();
    q.push(with_prio(Key{
        steps: 0,
        m: mm,
    }));

    let mut visited: BTreeSet<BTreeMap<Pos, (bool, Node)>> = BTreeSet::new();
    while let Some((_, k)) = q.pop() {
        if visited.contains(&k.m) {
            continue
        }
        visited.insert(k.m.clone());

        if let Some((g, _)) = k.m.get(&(0, 0)) {
            if *g { return k.steps }
        }

        let steps = 1 + k.steps;
        // consider all improving moves
        for (p, (v, n)) in &k.m {
            if n.used == 0 { continue }

            for (dx, dy) in [(-1, 0), (1, 0), (0, -1), (0, 1)] {
                let np = (p.0 + dx, p.1 + dy);
                if let Some((nv, nn)) = k.m.get(&np) {
                    if nn.avail < n.used { continue }

                    let mut m = k.m.clone();
                    m.insert(*p,(false, Node{
                        used: 0,
                        avail: n.used + n.avail,
                    }));
                    m.insert(np, (*nv || *v, Node{
                        used: nn.used + n.used,
                        avail: nn.avail - n.used,
                    }));

                    q.push(with_prio(Key{
                        steps,
                        m,
                    }));
                }
            }
        }
    }

    panic!("No solution!")
}

fn main() {
    let i = std::fs::read_to_string("input").expect("input");
    println!("P1 ans: {}", solve_p1(&i));

    let e = std::fs::read_to_string("example").expect("example");
    println!("P2 example: {}", solve_p2(&e));
    println!("P2 ans: {}", solve_p2(&i));
}
