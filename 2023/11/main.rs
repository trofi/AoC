use std::collections::HashSet;

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
struct Coord {
    x: isize,
    y: isize,
}

type Map = HashSet<Coord>;

fn parse_input(i: &str) -> Map {
    let mut m: Map = Map::new();

    for (y, l) in i.lines().enumerate() {
        for (x, c) in l.chars().enumerate() {
            if c == '#' {
                m.insert(Coord{x: x as isize, y: y as isize});
            }
        }
    }

    m
}

fn solve(i: &str, exp: usize) -> usize {
    let m = parse_input(i);

    // find empty rows and columns
    let min_x = m.iter().map(|c| c.x).min().expect("one");
    let max_x = m.iter().map(|c| c.x).max().expect("one");
    let min_y = m.iter().map(|c| c.y).min().expect("one");
    let max_y = m.iter().map(|c| c.y).max().expect("one");

    let empty_xs: HashSet<isize> =
        (min_x..=max_x).into_iter().filter(|x|
            m.iter().all(|c| c.x != *x)
        ).collect();
    let empty_ys: HashSet<isize> =
        (min_y..=max_y).into_iter().filter(|y|
            m.iter().all(|c| c.y != *y)
        ).collect();

    let mut r = 0;

    for lc in m.iter() {
        for rc in m.iter() {
            if lc == rc { continue }

            let fx = std::cmp::min(rc.x, lc.x);
            let tx = std::cmp::max(rc.x, lc.x);
            let fy = std::cmp::min(rc.y, lc.y);
            let ty = std::cmp::max(rc.y, lc.y);

            for x in fx..tx {
                r += if empty_xs.contains(&x) { exp } else { 1 };
            }

            for y in fy..ty {
                r += if empty_ys.contains(&y) { exp } else { 1 };
            }
        }
    }

    r / 2
}

fn main() {
    let e = std::fs::read_to_string("example").expect("example");
    let i = std::fs::read_to_string("input").expect("input");
    println!("P1 example: {}", solve(&e, 2));
    println!("P1   input: {}", solve(&i, 2));
    println!("P2  example10: {}", solve(&e, 10));
    println!("P2 example100: {}", solve(&e, 100));
    println!("P2      input: {}", solve(&i, 1000000));
}
