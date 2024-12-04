use std::collections::HashMap;

type Coord = (isize, isize);
type Map = HashMap::<Coord, char>;

fn parse_input(i: &str) -> Map {
    let mut m = Map::new();

    for (y, l) in i.lines().enumerate() {
        for (x, c) in l.chars().enumerate() {
            m.insert((x as isize, y as isize), c);
        }
    }

    m
}

fn get_word(pos: &Coord, dir: &Coord, l: usize, m: &Map) -> String {
    let mut r = String::new();

    for d in 0..(l as isize) {
        let p = (pos.0 + dir.0 * d, pos.1 + dir.1 * d);
        match m.get(&p) {
            Some (c) => r.push(*c),
            None => break,
        }
    }

    r
}

fn solve_p1(i: &str) -> usize {
    let m = parse_input(i);

    let w = "XMAS";

    let dirs: &[Coord] = &[
        (1, 0),  (1, 1), (0, 1), (-1, 1),
        (-1, 0), (-1, -1), (0, -1), (1, -1),
    ];

    let mut r = 0;

    for p in m.keys() {
        for d in dirs {
            if get_word(&p, d, w.len(), &m) == "XMAS" {
                r += 1;
            }
        }
    }

    r
}

fn solve_p2(i: &str) -> usize {
    let m = parse_input(i);

    let mut r = 0;

    let ws: &[&str] = &["MAS", "SAM"];

    for p in m.keys() {
        let w1 = get_word(&(p.0 - 1, p.1 - 1), &(1,  1), 3, &m);
        let w2 = get_word(&(p.0 - 1, p.1 + 1), &(1, -1), 3, &m);

        if ws.contains(&w1.as_str()) && ws.contains(&w2.as_str()) {
            r += 1;
        }
    }

    r
}

fn main() {
    let e = std::fs::read_to_string("example").expect("example");
    let i = std::fs::read_to_string("input").expect("input");
    println!("P1 e: {:?}", solve_p1(&e));
    println!("P1 i: {:?}", solve_p1(&i));
    println!("P2 e: {:?}", solve_p2(&e));
    println!("P2 i: {:?}", solve_p2(&i));
}
