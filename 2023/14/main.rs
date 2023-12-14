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

const NORTH: Coord = ( 0, -1);
const SOUTH: Coord = ( 0,  1);
const  WEST: Coord = (-1,  0);
const  EAST: Coord = ( 1,  0);

fn float_to(mut m: Map, dir: &Coord) -> Map {
    loop {
        let mut modified = false;
        for (p, c) in m.iter() {
            if *c != 'O' { continue }
            let np = (p.0 + dir.0, p.1 + dir.1);
            if m.get(&np) != Some(&'.') { continue }

            m.insert(*p, '.');
            m.insert(np, 'O');
            modified = true;
            break;
        }
        if !modified { break }
    }

    m
}

fn sum_up(m: &Map) -> isize {
    let max_y = m.keys().map(|p| p.1).max().expect("at least one") + 1;

    m.iter().filter(|(_, c)| **c == 'O').map(|(p, _)| max_y - p.1).sum()
}

fn solve_p1(i: &str) -> isize {
    let mut m = parse_input(i);

    m = float_to(m, &NORTH);

    sum_up(&m)
}

fn solve_p2(i: &str) -> isize {
    let mut m = parse_input(i);

    let mut v: Vec<Map> = Vec::new();

    for i in 1.. {
        m = float_to(m, &NORTH);
        m = float_to(m, &WEST);
        m = float_to(m, &SOUTH);
        m = float_to(m, &EAST);

        if let Some(ix) = v.iter().position(|e| *e == m) {
            let niters = 1_000_000_000;
            let cycle_len = v.len() - ix;

            let final_ix = ix + (niters - i) % cycle_len;

            return sum_up(&v[final_ix]);
        }
        v.push(m.clone());
    }

    panic!("Unreachable");
}

fn main() {
    let e = std::fs::read_to_string("example").expect("example");
    let i = std::fs::read_to_string("input").expect("input");
    println!("P1 example: {}", solve_p1(&e));
    println!("P1   input: {}", solve_p1(&i));
    println!("P2 example: {}", solve_p2(&e));
    println!("P2   input: {}", solve_p2(&i));
}
