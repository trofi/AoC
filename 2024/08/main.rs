use std::collections::HashMap;
use std::collections::HashSet;
use std::iter::FromIterator;

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

fn get_antinodes(a: &Coord, b: &Coord, is_p1: bool, m: &Map) -> Vec<Coord> {
    let dx = b.0 - a.0;
    let dy = b.1 - a.1;

    if is_p1 {
        return vec![(a.0 - dx, a.1 - dy), (b.0 + dx, b.1 + dy)];
    }

    let mut r: Vec<Coord> = Vec::new();
    let mut next_a: Coord = *a;
    let mut next_b: Coord = *b;

    while m.contains_key(&next_a) || m.contains_key(&next_b) {
        r.push(next_a);
        r.push(next_b);

        next_a = (next_a.0 - dx, next_a.1 - dy);
        next_b = (next_b.0 + dx, next_b.1 + dy);
    }

    r
}

fn solve(i: &str, is_p1: bool) -> usize {
    let m = parse_input(i);

    let mut antinode_locs: HashSet<Coord> = HashSet::new();

    // collect all antenae types
    let types: HashSet<char> = HashSet::from_iter(
        m.values()
         .filter(|c| **c != '.')
         .cloned());

    for t in &types {
        let pv: Vec<Coord> = m
            .iter()
            .filter(|(_,v)| *v == t)
            .map(|(k, _)| *k)
            .collect();

        // iterate over all antenae pairs within the type
        for i in 0..pv.len() {
            for j in (i+1)..pv.len() {
                for a in get_antinodes(&pv[i], &pv[j], is_p1, &m).into_iter() {
                    if !m.contains_key(&a) { continue }

                    antinode_locs.insert(a);
                }
            }
        }
    }

    antinode_locs.len()
}

fn main() {
    let e = std::fs::read_to_string("example").expect("example");
    let i = std::fs::read_to_string("input").expect("input");
    println!("P1 e: {:?}", solve(&e, true));
    println!("P1 i: {:?}", solve(&i, true));
    println!("P2 e: {:?}", solve(&e, false));
    println!("P2 i: {:?}", solve(&i, false));
}
