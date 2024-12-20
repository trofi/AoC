use std::collections::HashMap;
use std::collections::HashSet;
use std::collections::VecDeque;

type Coord = (isize, isize);
type Map = HashMap<Coord, char>;
type Cheat = (Coord, Coord);
type Distances = HashMap<Coord, usize>;

fn parse_input(i: &str) -> Map {
    let mut m = Map::new();

    for (y, l) in i.lines().enumerate() {
        for (x, c) in l.chars().enumerate() {
            m.insert((x as isize, y as isize), c);
        }
    }

    m
}

// What neighbors cat we reach using cheats exclusively? using DFS
fn get_cheat_neighs(p: &Coord, m: &Map, cheat_len: usize) -> Distances {
    let mut r = Distances::new();

    let mut visited: HashSet<Coord> = HashSet::new();
    let mut queue: VecDeque<(Coord, usize, bool)> = VecDeque::new();
    queue.push_back((*p, 0, false));

    while let Some(k) = queue.pop_front() {
        if visited.contains(&k.0) { continue }
        visited.insert(k.0);

        if k.1 >= cheat_len { continue }

        if k.2 {
            r.insert(k.0, k.1);
        }

        for d in [(-1, 0), (1, 0), (0, -1), (0, 1)] {
            let np: Coord = (k.0.0 + d.0, k.0.1 + d.1);

            if let Some(c) = m.get(&np) {
                let is_wall_exit = *c != '#';
                queue.push_back((np, k.1 + 1, is_wall_exit));
            }
        }
    }

    r
}

fn get_shortest_path(start: &Coord, end: &Coord, m: &Map, cheat_len: usize, distances: &Distances, max_len: usize) -> HashMap<Option<Cheat>, usize> {
    let mut r: HashMap<Option<Cheat>, usize> = HashMap::new();

    let mut visited: HashSet<Coord> = HashSet::new();
    let mut queue: VecDeque<(Coord, Option<Cheat>, usize)> = VecDeque::new();
    queue.push_back((*start, None, 0));

    while let Some(k) = queue.pop_front() {
        if k.2 > max_len { continue }

        // It's a bit special as we reach here via cheats and as a result
        // DFS does not guarantee "first reached is the fastest".
        if k.0 == *end {
            if let Some(v) = r.get(&k.1) {
                // does not improve know solution
                if *v <= k.2 { continue }
            }
            r.insert(k.1, k.2);
            continue
        }

        if visited.contains(&k.0) { continue }
        visited.insert(k.0);

        // explore neighbours
        for d in [(-1, 0), (1, 0), (0, -1), (0, 1)] {
            let np: Coord = (k.0.0 + d.0, k.0.1 + d.1);
            let steps = k.2 + 1;

            if let Some(c) = m.get(&np) {
                // usual traversal
                if *c != '#' {
                    queue.push_back((np, None, steps));
                }
                // enable cheat, and calculate all the steps needed to
                // reach the end using 'distances' cache.
                for (cp, cs) in get_cheat_neighs(&np, m, cheat_len) {
                    if let Some(es) = distances.get(&cp) {
                        let cheat = Some((k.0, cp));
                        let steps = steps + cs + es;

                        queue.push_back((*end, cheat, steps));
                    }
                }
            }
        }
    }

    r
}

// Using BFS calculate shortest distance to 'to' from any point on the
// graph.
fn get_distances_to(to: &Coord, m: &Map) -> Distances {
    let mut r = Distances::new();

    let mut queue: VecDeque<(Coord, usize)> = VecDeque::new();
    queue.push_back((*to, 0));

    while let Some(k) = queue.pop_front() {
        if r.contains_key(&k.0) { continue }
        r.insert(k.0, k.1);

        for d in [(-1, 0), (1, 0), (0, -1), (0, 1)] {
            let np: Coord = (k.0.0 + d.0, k.0.1 + d.1);
            if let Some(c) = m.get(&np) {
                if *c != '#' {
                    queue.push_back((np, k.1 + 1));
                }
            }
        }
    }

    r
}

fn solve(i: &str, cheat_len: usize, delta: usize) -> usize {
    let m = parse_input(i);

    let s: &Coord = m.iter().filter(|(_,c)| **c == 'S').next().expect("a Start node").0;
    let e: &Coord = m.iter().filter(|(_,c)| **c == 'E').next().expect("an End node").0;

    // cache fastest path from a "cheat target" to the "end".
    let dm = get_distances_to(e, &m);
    let no_cheats = dm.get(s).expect("have a solution without the cheats");

    let rm = get_shortest_path(s, e, &m, cheat_len, &dm, no_cheats - delta);

    rm.len()
}

fn main() {
    let e = std::fs::read_to_string("example").expect("example");
    let i = std::fs::read_to_string("input").expect("input");
    println!("P1 e: {:?} (expect 2)", solve(&e, 2, 40));
    println!("P1 e: {:?} (expect 1)", solve(&e, 2, 64));
    println!("P1 i: {:?}", solve(&i, 2, 100));
    println!("P2 e: {:?} (expect 29)", solve(&e, 20, 72));
    println!("P2 e: {:?} (expect 7)", solve(&e, 20, 74));
    println!("P2 e: {:?} (expect 3)", solve(&e, 20, 76));
    println!("P2 i: {:?}", solve(&i, 20, 100));
}
