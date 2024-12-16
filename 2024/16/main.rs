use std::collections::HashMap;
use std::collections::BTreeSet;

type Coord = (isize, isize);
type Map = HashMap<Coord, char>;

#[derive(Clone, Copy, Eq, Hash, PartialEq)]
enum Dir { E, S, W, N, }

const DIRS: [Dir; 4] = [ Dir::E, Dir::S, Dir::W, Dir::N ];

fn parse_input(i: &str) -> Map {
    let mut m = Map::new();

    for (y, l) in i.lines().enumerate() {
        for (x, c) in l.chars().enumerate() {
            m.insert((x as isize, y as isize), c);
        }
    }

    m
}

#[derive(Debug)]
struct Result {
    cost: usize,
    tiles: usize,
}

fn solve_p1(i: &str) -> Result {
    let m = parse_input(i);

    let start: Coord = *m.iter().filter(|(_, v)| **v == 'S').next().expect("a start position").0;
    let end: Coord = *m.iter().filter(|(_, v)| **v == 'E').next().expect("an end start position").0;

    eprintln!("start={start:?} -> end={end:?}");

    let mut visited: HashMap<(Coord, Dir), usize> = HashMap::new();

    #[derive(Debug, Eq, Ord, PartialEq, PartialOrd)]
    struct Key {
        dist: isize,
        cost: usize, // also the score
        pos: Coord,
        dir: usize,
        seen_tiles: Vec<Coord>,
    }

    let mut queue: BTreeSet<Key> = BTreeSet::new();
    queue.insert(Key{
        dist: (start.0 - end.0).abs() + (start.1 - end.1).abs(),
        cost: 0,
        pos: start,
        dir: 0, // east
        seen_tiles: vec![start],
    });

    let mut best_cost = usize::MAX;
    let mut seen_tiles: BTreeSet<Coord> = BTreeSet::new();

    while let Some(k) = queue.pop_first() {
        //eprintln!("Consider {k:?}");
        if k.cost > best_cost { continue }
        if k.pos == end {
            if k.cost < best_cost {
                eprintln!("Found better: {best_cost} -> {}", k.cost);
                best_cost = k.cost;
                seen_tiles = BTreeSet::new();
            }
            /* k.cost == best_cost */
            for t in &k.seen_tiles {
                seen_tiles.insert(*t);
            }
            continue
        }
        if let Some(cost) = visited.get(&(k.pos, DIRS[k.dir])) {
            if *cost < k.cost {
                //eprintln!("Been here with: cost={cost} (k.cost={:?}); dropping", k.cost);
                continue
            }
        }
        visited.insert((k.pos, DIRS[k.dir]), k.cost);

        // rotate neighbors:
        queue.insert(Key{
            dist: k.dist,
            cost: k.cost + 1000,
            pos: k.pos,
            dir: (k.dir + 1) % DIRS.len(), // clockwise
            seen_tiles: k.seen_tiles.clone(),
        });
        queue.insert(Key{
            dist: k.dist,
            cost: k.cost + 1000,
            pos: k.pos,
            dir: (k.dir + DIRS.len() - 1) % DIRS.len(), // counterclockwisse
            seen_tiles: k.seen_tiles.clone(),
        });

        // advance neighbor
        let np = match DIRS[k.dir] {
            Dir::E => (k.pos.0 + 1, k.pos.1),
            Dir::N => (k.pos.0    , k.pos.1 + 1),
            Dir::W => (k.pos.0 - 1, k.pos.1),
            Dir::S => (k.pos.0    , k.pos.1 - 1),
        };
        if m.get(&np) != Some(&'#') {
            queue.insert(Key{
                dist: (np.0 - end.0).abs() + (np.1 - end.1).abs(),
                cost: k.cost + 1,
                pos: np,
                dir: k.dir,
                seen_tiles: { let mut s = k.seen_tiles.clone(); s.push(np); s },
            });
        }
    }

    Result {
        cost: best_cost,
        tiles: seen_tiles.len(),
    }
}

fn main() {
    let e = std::fs::read_to_string("example").expect("example");
    let i = std::fs::read_to_string("input").expect("input");
    println!("P1/2 e: {:?}", solve_p1(&e));
    println!("P1/2 i: {:?}", solve_p1(&i));
}
