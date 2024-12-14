use std::collections::HashSet;
use std::iter::FromIterator;

type Coord = (isize, isize);

#[derive(Debug)]
struct Robot {
    p: Coord,
    v: Coord,
}

fn parse_robot(i: &str) -> Robot {
    let es: Vec<&str> = i
        .split(&['=', ',', ' '])
        .filter(|e| *e != "")
        .collect();

    match es.as_slice() {
        ["p", pxs, pys, "v", vxs, vys] => Robot{
            p: (pxs.parse::<isize>().expect("a number (px)"),
                pys.parse::<isize>().expect("a number (py)")),
            v: (vxs.parse::<isize>().expect("a number (vx)"),
                vys.parse::<isize>().expect("a number (vy)")),
        },
        _ => panic!("Unhandled {:?} entry", es),
    }
}

fn parse_input(i: &str) -> Vec<Robot> {
    i.lines().map(|l| parse_robot(l)).collect()
}

fn advance(r: &Robot, dim: &Coord, steps: isize) -> Coord {
    let x = (r.p.0 + r.v.0 * steps).rem_euclid(dim.0);
    let y = (r.p.1 + r.v.1* steps).rem_euclid(dim.1);

    (x, y)
}

// Quadrants:
//   0 | 1
//   --+--
//   2 | 3
// Or None.
fn quad(p: &Coord, dim: &Coord) -> Option<usize> {
    let mx = dim.0 / 2;
    let my = dim.1 / 2;

    if p.0 == mx || p.1 == my {
        return None
    }

    let x = (p.0 > mx) as usize;
    let y = (p.1 > my) as usize;

    Some(x + 2 * y)
}

fn solve_p1(i: &str, dim: &Coord, steps: isize) -> usize {
    let rs = parse_input(i);

    let mut qs: [usize; 4] = [0, 0, 0, 0];

    for r in &rs {
        let p = advance(r, dim, steps);
        if let Some(q) = quad(&p, dim) {
            qs[q] += 1;
        }
    }

    IntoIterator::into_iter(qs).product()
}

fn largest_scc(m: &HashSet<Coord>) -> usize {
    // how many robots can we reach from this robot?
    let mut sccs: Vec<HashSet<Coord>> = Vec::new();
    let mut visited: HashSet<Coord> = HashSet::new();
    let mut q: Vec<Coord> = Vec::new();

    for e in m {
        if visited.contains(e) { continue }
        let mut scc: HashSet<Coord> = HashSet::new();

        q.push(*e);
        while let Some(p) = q.pop() {
            if visited.contains(&p) { continue }
            visited.insert(p);
            scc.insert(p);

            // look up neighbors
            for dx in -1..=1 {
                for dy in -1..=1 {
                    let np = (p.0 + dx, p.1 + dy);
                    if !m.contains(&np) { continue }
                    if visited.contains(&np) { continue }
                    q.push(np);
                }
            }
        }

        sccs.push(scc);
    }

    let mut slv: Vec<usize> = sccs.iter().map(|s| s.len()).collect();
    slv.sort();
    slv[slv.len() - 1] // pick largest scc size
}

fn solve_p2(i: &str, dim: &Coord) -> isize {
    let rs = parse_input(i);
    let start_ps: Vec<Coord> = rs.iter().map(|r| r.p).collect();

    let mut scc_seen: usize = 0;
    let mut step_seen: isize = -1;

    for step in 1.. {
        let ps: Vec<Coord> = rs.iter().map(|r| advance(r, dim, step)).collect();

        // traversed everything
        if ps == start_ps { break}

        let map: HashSet<Coord> = HashSet::from_iter(ps);

        // check for largest cluster
        let new_scc = largest_scc(&map);
        if new_scc <= scc_seen { continue }
        scc_seen = new_scc;
        step_seen = step;

        println!("===--- step={step} scc_seen={scc_seen} ---===");
        for y in 0..dim.1 {
            for x in 0..dim.0 {
                if map.contains(&(x, y)) {
                    print!("#");
                } else {
                    print!(".");
                }
            }
            println!();
        }
    }

    step_seen
}

fn main() {
    let e = std::fs::read_to_string("example").expect("example");
    let i = std::fs::read_to_string("input").expect("input");
    println!("P1 e: {:?}", solve_p1(&e, &(11, 7), 100));
    println!("P1 i: {:?}", solve_p1(&i, &(101, 103), 100));
    println!("P2 e: {:?}", solve_p2(&e, &(11, 7)));
    println!("P2 i: {:?}", solve_p2(&i, &(101, 103)));
}
