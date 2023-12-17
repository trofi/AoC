use std::collections::HashMap;
use std::collections::BTreeSet;

type Coord = (isize, isize);
type Map = HashMap<Coord, usize>;

fn parse_input(i: &str) -> Map {
    let mut m = Map::new();

    for (y, l) in i.lines().enumerate() {
        for (x, c) in l.chars().enumerate() {
            let d: usize = c.to_digit(10).expect("a digit") as usize;
            m.insert((x as isize, y as isize), d);
        }
    }

    m
}
fn get_dirs(dir: &Coord) -> Vec<Coord> {
    match dir {
        ( 0, -1) => vec![(-1, 0), ( 0,-1), ( 1, 0)],
        ( 0,  1) => vec![(-1, 0), ( 0, 1), ( 1, 0)],
        ( 1,  0) => vec![( 0,-1), ( 1, 0), ( 0, 1)],
        (-1,  0) => vec![( 0, 1), (-1, 0), (0, -1)],
        _ => panic!("Unhandled direction {:?}", dir),
    }
}

fn solve_p1(i: &str) -> usize {
    let m = parse_input(i);
    let end_p = *m.keys().max().expect("one");

    #[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
    struct Entry {
        // for each element lower is higher in priority queue
        cost: usize,
        stride: usize,
        pos: Coord,
        dir: Coord,
    }

    fn make_entry(cost: usize, stride: usize, pos: Coord, dir: Coord) -> Entry {
        Entry {
            cost,
            stride,
            pos,
            dir,
        }
    }

    let mut q: BTreeSet<Entry> = BTreeSet::new();
    // (pos, dir. stride) => cost
    let mut seen: HashMap<(Coord, Coord, usize), usize> = HashMap::new();

    let mut r = usize::MAX;

    q.insert(make_entry(0,0,(0,0),(1,0)));

    while let Some(k) = q.pop_first() {
        if k.cost >= r { continue }
        if k.stride > 3 { continue }
        if k.pos == end_p { r = k.cost; }

        if let Some(c) = seen.get(&(k.pos, k.dir, k.stride)) {
            if *c <= k.cost { continue }
        }
        seen.insert((k.pos, k.dir, k.stride), k.cost);

        for dir in get_dirs(&k.dir) {
            let pos = (k.pos.0 + dir.0, k.pos.1 + dir.1);
            let stride = if dir == k.dir { k.stride + 1 } else { 1 };

            if let Some(d) = m.get(&pos) {
                let cost = k.cost + d;

                q.insert(make_entry(cost,stride,pos,dir));
            }
        }
    }

    r
}

fn solve_p2(i: &str) -> usize {
    let m = parse_input(i);
    let end_p = *m.keys().max().expect("one");

    #[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
    struct Entry {
        // for each element lower is higher in priority queue
        cost: usize,
        stride: usize,
        pos: Coord,
        dir: Coord,
    }

    fn make_entry(cost: usize, stride: usize, pos: Coord, dir: Coord) -> Entry {
        Entry {
            cost,
            stride,
            pos,
            dir,
        }
    }

    let mut q: BTreeSet<Entry> = BTreeSet::new();
    // (pos, dir. stride) => cost
    let mut seen: HashMap<(Coord, Coord, usize), usize> = HashMap::new();

    let mut r = usize::MAX;

    q.insert(make_entry(0,0,(0,0),(1,0)));
    q.insert(make_entry(0,0,(0,0),(0,1)));

    while let Some(k) = q.pop_first() {
        if k.cost >= r { continue }
        if k.stride > 10 { continue }
        if k.pos == end_p && k.stride >=4 { r = k.cost; }

        if let Some(c) = seen.get(&(k.pos, k.dir, k.stride)) {
            if *c <= k.cost { continue }
        }
        seen.insert((k.pos, k.dir, k.stride), k.cost);

        for dir in get_dirs(&k.dir) {
            if k.stride < 4 && dir != k.dir { continue }
            let pos = (k.pos.0 + dir.0, k.pos.1 + dir.1);
            let stride = if dir == k.dir { k.stride + 1 } else { 1 };

            if let Some(d) = m.get(&pos) {
                let cost = k.cost + d;

                q.insert(make_entry(cost,stride,pos,dir));
            }
        }
    }

    r
}

fn main() {
    let e = std::fs::read_to_string("example").expect("example");
    let i = std::fs::read_to_string("input").expect("input");
    println!("P1 example: {}", solve_p1(&e));
    println!("P1   input: {}", solve_p1(&i));
    println!("P2 example: {}", solve_p2(&e));
    println!("P2   input: {}", solve_p2(&i));
}
