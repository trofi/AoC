use std::collections::HashMap;

type Coord = (isize, isize);
type Map = HashMap<Coord, char>;

fn parse_input(i: &str) -> Map {
    let mut m = Map::new();

    for (y, l) in i.lines().enumerate() {
        for (x, c) in l.chars().enumerate() {
            m.insert((x as isize, y as isize), c);
        }
    }

    m
}

fn solve(i: &str, dx: isize, dy: isize) -> usize {
    let m = parse_input(i);

    let dim = m.keys().max().expect("at least one point");

    (0..=dim.1).filter(|n| m.get(&((dx * *n) % (dim.0 + 1), dy * *n)) == Some(&'#')).count()
}

fn solve_p1(i: &str) -> usize {
    solve(i, 3, 1)
}

fn solve_p2(i: &str) -> usize {
    [(1,1), (3,1), (5, 1), (7, 1), (1, 2)]
        .iter()
        .map(|(dx, dy)| solve(i, *dx, *dy))
        .product()
}

fn main() {
    let e = std::fs::read_to_string("example").expect("example");
    let i = std::fs::read_to_string("input").expect("input");

    println!("P1 e: {:?}", solve_p1(&e));
    println!("P1 i: {:?}", solve_p1(&i));
    println!("P2 e: {:?}", solve_p2(&e));
    println!("P2 i: {:?}", solve_p2(&i));
}
