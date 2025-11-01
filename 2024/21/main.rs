use std::collections::HashMap;
use std::collections::VecDeque;

fn parse_input(i: &str) -> Vec<(Vec<char>, usize)> {
    i.lines()
     .map(|l| (l.chars().collect(), l[0..l.len()-1].parse::<usize>().expect("a number")))
     .collect()
}

#[derive(Clone, Copy, Eq, Hash, PartialEq)]
struct Path {
    from: char,
    to: char,
}

// Path is a src->dst on control keyboard with an activation
// 'usize' is a cost.
type CostMap = HashMap<Path, usize>;

type Coord = (isize, isize);

const DIRPAD: [(Coord, char); 5] = [
                  ((1,1), '^'), ((2,1), 'A'),
    ((0,0), '<'), ((1,0), 'v'), ((2,0), '>'),
];

const NUMPAD: [(Coord, char); 11] = [
    ((0,3), '7'), ((1,3), '8'), ((2,3), '9'),
    ((0,2), '4'), ((1,2), '5'), ((2,2), '6'),
    ((0,1), '1'), ((1,1), '2'), ((2,1), '3'),
                  ((1,0), '0'), ((2,0), 'A'),
];

type Pad = HashMap<Coord, char>;

fn dir2delta(dir: char) -> Coord {
    match dir {
        'A' => ( 0, 0),
        'v' => ( 0,-1),
        '^' => ( 0, 1),
        '<' => (-1, 0),
        '>' => ( 1, 0),
        _ => panic!("Unhandled {} direction", dir)
    }
}

fn advance_dir_one(pos: char, dir: char, pad: &Pad) -> Option<char> {

    let d = dir2delta(dir);
    
    for (c, v) in pad.iter() {
        if pos == *v {
            let np = (c.0 + d.0, c.1 + d.1);
            return pad.get(&np).copied()
        }
    }
    panic!("Unresolved pos={}", pos);
}

// Derive movement costmap from previous (input) to the
// next (output) controller.
fn advance_pad(m: &CostMap, pad: &Pad) -> CostMap {
    struct State {
        spath: Path,
        dpath: Path,
        cost: usize,
        stepped: bool,
    }

    let mut r = CostMap::new();
    let mut seen: HashMap<(Path, Path), usize> = HashMap::new();
    let mut q = VecDeque::new();
    for pos in pad.values() {
        q.push_back(State{
            spath: Path{from: 'A', to: 'A'},
            dpath: Path{from: *pos, to: *pos},
            cost: 0,
            stepped: false,
        });
    }

    while let Some(e) = q.pop_front() {
        if e.stepped {
            if let Some(seen_cost) = seen.get(&(e.spath, e.dpath)) {
                if *seen_cost <= e.cost { continue }
            }
            seen.insert((e.spath, e.dpath), e.cost);
            if e.spath.to == 'A' {
                r.insert(e.dpath, e.cost);
                continue
            }
        }

        // explore all moves
        for (p, pcost) in m {
            if p.from != e.spath.to { continue }

            if let Some(d) = advance_dir_one(e.dpath.to, e.spath.to, pad) {
                q.push_back(State{
                    spath: Path{from: e.spath.from, to: p.to},
                    dpath: Path{from: e.dpath.from, to: d},
                    cost: e.cost + pcost,
                    stepped: true,
                });
            }
        }
    }

    r
}

fn solve_one(i: &[char], m: &CostMap) -> usize {
    let mut r = 0;

    let mut prev: char = 'A';

    for c in i {
        let path = Path{from: prev, to: *c};
        let d = match m.get(&path) {
            Some(cost) => *cost,
            _          => panic!("Unexpected path request"),
        };
        r += d;
        prev = *c;
    }

    r
}

fn solve(i: &str, depth: usize) -> usize {
    let dirpad: Pad = Pad::from(DIRPAD);
    let numpad: Pad = Pad::from(NUMPAD);

    let mut m: CostMap = CostMap::new();
    // Any button click is the cheapest one-button press.
    for from in dirpad.values() {
        for to in dirpad.values() {
            m.insert(Path{from: *from, to: *to}, 1);
        }
    }

    for _ in 0..depth {
        m = advance_pad(&m, &dirpad);
    }

    m = advance_pad(&m, &numpad);

    let cs = parse_input(i);

    cs.into_iter()
      .map(|c| solve_one(&c.0, &m) * c.1)
      .sum()
}

fn main() {
    let e = std::fs::read_to_string("example").expect("example");
    let i = std::fs::read_to_string("input").expect("input");
    println!("P1 e: {:?} (expect 126384)", solve(&e, 2));
    println!("P1 i: {:?}", solve(&i, 2));
    println!("P2 i: {:?}", solve(&i, 25));
}
