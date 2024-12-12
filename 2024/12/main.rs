use std::collections::HashSet;

type Coord = (isize, isize);
type Map = HashSet<(Coord, char)>;
type Region = (char, HashSet<Coord>);

fn parse_input(i: &str) -> Map {
    let mut m = Map::new();

    for (y, l) in i.lines().enumerate() {
        for (x, c) in l.chars().enumerate() {
            m.insert(((x as isize, y as isize), c));
        }
    }

    m
}

fn get_neighbors(c: &Coord) -> Vec<(Coord, char)> {
    vec![
        ((c.0 - 1, c.1), 'l'),
        ((c.0 + 1, c.1), 'r'),
        ((c.0, c.1 - 1), 'u'),
        ((c.0, c.1 + 1), 'd'),
    ]
}

fn get_regions(m: &Map) -> Vec<Region> {
    let mut res: Vec<Region> = Vec::new();

    let mut visited: HashSet<(Coord, char)> = HashSet::new();

    for (sc, t) in m {
        if visited.contains(&(*sc, *t)) { continue }

        let mut r: Region = (*t, HashSet::new());
        r.1.insert(*sc);

        let mut queue: Vec<Coord> = vec![*sc];

        // traverse neighbours of the same type
        while let Some(c) = queue.pop() {
            if visited.contains(&(c, *t)) { continue }
            visited.insert((c, *t));
            r.1.insert(c);

            for (nc, _nt) in get_neighbors(&c).into_iter() {
                if m.contains(&(nc, *t)) {
                    queue.push(nc);
                }
            }
        }

        res.push(r);
    }

    res
}

fn get_perimeter((_, cs): &Region) -> Map {
    let mut r = Map::new();

    for c in cs {
        for (nc, nt) in get_neighbors(c).into_iter() {
            if !cs.contains(&nc) {
                r.insert((*c, nt));
            }
        }
    }

    r
}

fn solve_p1(i: &str) -> usize {
    let m = parse_input(i);

    let regions = get_regions(&m);

    regions
        .into_iter()
        .map(|r| r.1.len() * get_perimeter(&r).len())
        .sum()
}

fn solve_p2(i: &str) -> usize {
    let m = parse_input(i);

    let regions = get_regions(&m);

    regions
        .into_iter()
        .map(|r| r.1.len() * get_regions(&get_perimeter(&r)).len())
        .sum()
}

fn main() {
    let e = std::fs::read_to_string("example").expect("example");
    let i = std::fs::read_to_string("input").expect("input");
    println!("P1 e: {:?}", solve_p1(&e));
    println!("P1 i: {:?}", solve_p1(&i));
    println!("P2 e: {:?}", solve_p2(&e));
    println!("P2 i: {:?}", solve_p2(&i));
}
