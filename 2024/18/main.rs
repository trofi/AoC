use std::collections::BTreeSet;
use std::collections::HashMap;
use std::collections::HashSet;
use std::iter::FromIterator;

type Coord = (isize, isize);
type Map = HashSet<Coord>;

fn parse_input(i: &str) -> Vec<Coord> {
    i.lines().map(|e| {
        let (xs, ys) = e.split_once(',').expect("X,Y");
         let x = xs.parse::<isize>().expect("X is a number");
         let y = ys.parse::<isize>().expect("Y is a number");

         (x, y)
    }).collect()
}

fn solve_p1(i: &str, sim_fall: usize, dim: &Coord) -> Option<usize> {
    let vs = parse_input(i);

    let s = (0, 0);
    let e = (dim.0, dim.1);

    let m = Map::from_iter(vs[..sim_fall].into_iter().cloned());

    #[derive(Debug, Eq, Ord, PartialEq, PartialOrd)]
    struct Key {
        prio: usize, // lower is better
        steps: usize,
        pos: Coord,
    }
    fn mk_key(pos: &Coord, steps: usize, dim: &Coord) -> Key {
        // lowest score is the closest to the best result
        let prio = steps + ((dim.0 - pos.0) as usize) + ((dim.1 - pos.1) as usize);

        Key {
            prio,
            steps,
            pos: *pos,
        }
    }

    let mut visited: HashMap<Coord, usize> = HashMap::new();
    let mut queue: BTreeSet<Key> = BTreeSet::new();
    queue.insert(mk_key(&s, 0, dim));

    while let Some(key) = queue.pop_first() {
        if let Some(seen_steps) = visited.get(&key.pos) {
            if *seen_steps <= key.steps { continue }
        }
        visited.insert(key.pos, key.steps);

        // Pick the first solution as we construct it to be the first
        // to explore thanks to `prio` structure.
        if key.pos == e { return Some(key.steps) }

        // Explore the neighbors.
        let steps = key.steps + 1;
        for dn in [(-1, 0), (1, 0), (0, -1), (0, 1)] {
            let np = (key.pos.0 + dn.0, key.pos.1 + dn.1);

            // out if bounds
            if np.0 < 0 || np.0 > dim.0 { continue }
            if np.1 < 0 || np.1 > dim.1 { continue }
            // corrupted block
            if m.contains(&np) { continue }

            queue.insert(mk_key(&np, steps, dim));
        }
    }

    None
}

fn solve_p2(i: &str, dim: &Coord) -> Coord {
    let vs = parse_input(i);

    let ivs: Vec<usize>= (0..vs.len()).collect();
    let ix = ivs.partition_point(|ix| solve_p1(i, ix+1, dim).is_some());
    return vs[ix];
}

fn main() {
    let e = std::fs::read_to_string("example").expect("example");
    let i = std::fs::read_to_string("input").expect("input");
    println!("P1 e: {:?}", solve_p1(&e, 12, &(6, 6)));
    println!("P1 i: {:?}", solve_p1(&i, 1024, &(70, 70)));
    println!("P2 e: {:?}", solve_p2(&e, &(6, 6)));
    println!("P2 i: {:?}", solve_p2(&i, &(70, 70)));
}
