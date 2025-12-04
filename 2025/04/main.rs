use std::collections::HashMap;
use std::collections::VecDeque;
use std::iter::FromIterator;

#[derive(Debug)]
enum E {
    Input(&'static str, std::io::Error),
    Parse(&'static str, char),
}

type Coord = (isize, isize);
type Map = HashMap<Coord, char>;

fn parse_input(i: &str) -> Result<Map, E> {
    let mut m = Map::new();

    for (y, l) in i.lines().enumerate() {
        for (x, c) in l.chars().enumerate() {
            if c == '.' { continue }
            if c == '@' {
                m.insert((x as isize, y as isize), c);
            } else {
                return Err(E::Parse("Unexpected tile type", c));
            }
        }
    }

    Ok(m)
}

fn neighs(c: &Coord, m: &Map) -> Vec<Coord> {
    let mut r: Vec<Coord> = vec![];

    for dx in -1..=1 {
        for dy in -1..=1 {
            let nc = (c.0 + dx, c.1 + dy);
            if nc == *c { continue }
            if let Some('@') = m.get(&nc) {
                r.push(nc);
            }
        }
    }

    r
}

fn solve_p1(i: &str) -> Result<usize, E> {
    let m = parse_input(i)?;

    let mut r = 0;

    for ((x, y), c) in m.iter() {
        if *c != '@' { continue }

        if neighs(&(*x, *y), &m).len() < 4 {
            r += 1;
        }
    }

    Ok(r)
}

fn solve_p2(i: &str) -> Result<usize, E> {
    let mut m = parse_input(i)?;

    let mut s = m.len();

    let mut q: VecDeque<Coord> = VecDeque::from_iter(m.keys().cloned());

    while let Some(c) = q.pop_front() {
        let ns = neighs(&c, &m);
        if m.contains_key(&c) && ns.len() < 4 {
            m.remove(&c);
            // revisit neighbors if we deleted an entry
            for n in &ns { q.push_back(*n) }
        }
    }

    Ok(s - m.len())
}

fn main() -> Result<(), E> {
    let e = std::fs::read_to_string("example")
        .map_err(|e| E::Input("example", e))?;
    let i = std::fs::read_to_string("input")
        .map_err(|e| E::Input("input", e))?;

    println!("P1 e: {:?}", solve_p1(&e)?);
    println!("P1 i: {:?}", solve_p1(&i)?);

    println!("P2 e: {:?}", solve_p2(&e)?);
    println!("P2 i: {:?}", solve_p2(&i)?);
    Ok(())
}
