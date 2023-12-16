use std::collections::HashMap;
use std::collections::HashSet;
use std::collections::VecDeque;

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

fn count_energised(m: &Map, ip: &Coord, id: &Coord) -> usize {
    // (pos, dir)
    let mut visited: HashSet<(Coord, Coord)> = HashSet::new();
    let mut q: VecDeque<(Coord, Coord)> = VecDeque::new();

    q.push_back((*ip, *id));

    while let Some(v) = q.pop_front() {
        if visited.contains(&v) { continue }
        visited.insert(v);

        let (p, d) = v;
        let np = (p.0 + d.0, p.1 + d.1);
        if let Some(c) = m.get(&np) {
            match (c, d) {
                // splits:
                ('|', (_, 0)) => {
                    q.push_back((np, (0, -1)));
                    q.push_back((np, (0,  1)));
                },
                ('-', (0, _)) => {
                    q.push_back((np, (-1, 0)));
                    q.push_back((np, ( 1, 0)));
                },
                // passes through
                ('.', _) | ('|', (0, _)) | ('-', (_, 0)) => {
                    q.push_back((np, d));
                },
                // reflections, symmetric WRT direction change:
                ('/',  (-1, 0)) => { q.push_back((np, (0,  1))); }
                ('/',  (1,  0)) => { q.push_back((np, (0, -1))); }
                ('/',  (0, -1)) => { q.push_back((np, (1,  0))); }
                ('/',  (0,  1)) => { q.push_back((np, (-1, 0))); }
                ('\\', (0, -1)) => { q.push_back((np, (-1, 0))); }
                ('\\', (0,  1)) => { q.push_back((np, (1,  0))); }
                ('\\', (-1, 0)) => { q.push_back((np, (0, -1))); }
                ('\\', (1,  0)) => { q.push_back((np, (0,  1))); }
                _ => panic!("Unhandled {:?} tile", (c, d)),
            };
        }
    }

    // "-1" as we start just "outside" the field
    visited.into_iter().map(|(p,_)| p).collect::<HashSet<_>>().len() - 1
}

fn solve_p1(i: &str) -> usize {
    let m = parse_input(i);

    count_energised(&m, &(-1, 0), &(1, 0))
}

fn solve_p2(i: &str) -> usize {
    let m = parse_input(i);

    let max_x: isize = *m.keys().map(|(x,_)| x).max().expect("one") + 1;
    let max_y: isize = *m.keys().map(|(_,y)| y).max().expect("one") + 1;

    let mut rs: Vec<usize> = Vec::new();

    for x in 0..max_x {
        // top to bottom
        rs.push(count_energised(&m, &(x,    -1), &(0,  1)));
        // bottom to top
        rs.push(count_energised(&m, &(x, max_y), &(0, -1)));
    }

    for y in 0..max_y {
        // left to right
        rs.push(count_energised(&m, &(   -1, y), &( 1, 0)));
        // right to left
        rs.push(count_energised(&m, &(max_x, y), &(-1, 0)));
    }

    rs.into_iter().max().expect("one")
}

fn main() {
    let e = std::fs::read_to_string("example").expect("example");
    let i = std::fs::read_to_string("input").expect("input");
    println!("P1 example: {}", solve_p1(&e));
    println!("P1   input: {}", solve_p1(&i));
    println!("P2 example: {}", solve_p2(&e));
    println!("P2   input: {}", solve_p2(&i));
}
