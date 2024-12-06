use std::collections::HashMap;
use std::collections::HashSet;

type Coord = (isize, isize);
type Map = HashMap<Coord, char>;
type PatroledArea = HashSet<Coord>;

fn parse_input(i: &str) -> Map {
    let mut m = Map::new();

    for (y, l) in i.lines().enumerate() {
        for (x, c) in l.chars().enumerate() {
            m.insert((x as isize, y as isize), c);
        }
    }

    m
}

fn start(m: &Map) -> Coord {
    for (p, c) in m {
        if *c == '^' { return *p }
    }

    panic!("No '^' start pos found")
}

const DIRS: &[Coord] = &[
    (0, -1), /* up */
    (1, 0),
    (0, 1),
    (-1, 0),
];

fn solve_p1(i: &str) -> PatroledArea {
    let m = parse_input(&i);

    let mut visited: PatroledArea = HashSet::new();
    let mut d = 0;
    let mut p = start(&m);

    loop {
        visited.insert(p);
        let np = (p.0 + DIRS[d].0, p.1 + DIRS[d].1);

        match m.get(&np) {
            None => break,
            Some('#') => { d = (d + 1) % DIRS.len(); },
            Some('.') | Some('^') => { p = np; }
            v => panic!("Unhandled {:?} input", v),
        }
    }

    visited
}

fn solve_p2(i: &str) -> usize {
    let m = parse_input(&i);
    let s = start(&m);
    let patroled = solve_p1(&i);

    let mut obstacles: Vec<Coord> = Vec::new();

    // place obstacles only at places of traversed locations
    'candidate_loop: for o in &patroled {
        if *o == s { continue }

        let mut m = m.clone();
        m.insert(*o, '#');

        // visited key is (pos, dir) to detect loops.
        let mut visited: HashSet<(Coord, usize)> = HashSet::new();
        let mut d = 0;
        let mut p = s;
        loop {
            if visited.contains(&(p, d)) {
                // found loop!
                obstacles.push(*o);
                break;
            }
            visited.insert((p, d));
            let np = (p.0 + DIRS[d].0, p.1 + DIRS[d].1);

            match m.get(&np) {
                None => continue 'candidate_loop,
                Some('#') => { d = (d + 1) % DIRS.len(); },
                Some('.') | Some('^') => { p = np; }
                v => panic!("Unhandled {:?} input", v),
            }
        }
    }

    obstacles.len()
}

fn main() {
    let e = std::fs::read_to_string("example").expect("example");
    let i = std::fs::read_to_string("input").expect("input");
    println!("P1 e: {:?}", solve_p1(&e).len());
    println!("P1 i: {:?}", solve_p1(&i).len());
    println!("P2 e: {:?}", solve_p2(&e));
    println!("P2 i: {:?}", solve_p2(&i));
}
