use std::collections::HashMap;
use std::collections::VecDeque;

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
struct Coord {
    x: isize,
    y: isize,
}

type Map = HashMap<Coord, char>;

// Known states: "|-LJ7FS"

fn is_to_left(c: char) -> bool { "-J7S".contains(c) }
fn is_to_right(c: char) -> bool { "-LFS".contains(c) }
fn is_to_up(c: char) -> bool { "|LJS".contains(c) }
fn is_to_down(c: char) -> bool { "|7FS".contains(c) }

fn get_neighbors(p: &Coord, m: &Map) -> Vec<Coord> {
    let mut res: Vec<Coord> = Vec::new();

    let c = *m.get(p).unwrap_or(&'.');
    let l = Coord{x: p.x - 1, y: p.y};
    let r = Coord{x: p.x + 1, y: p.y};
    let u = Coord{x: p.x, y: p.y - 1};
    let d = Coord{x: p.x, y: p.y + 1};
    let lc = *m.get(&l).unwrap_or(&'.');
    let rc = *m.get(&r).unwrap_or(&'.');
    let uc = *m.get(&u).unwrap_or(&'.');
    let dc = *m.get(&d).unwrap_or(&'.');

    if is_to_left(c)  && is_to_right(lc) { res.push(l); }
    if is_to_right(c) && is_to_left(rc)  { res.push(r); }
    if is_to_up(c)    && is_to_down(uc)  { res.push(u); }
    if is_to_down(c)  && is_to_up(dc)    { res.push(d); }

    res
}

fn parse_input(i: &str) -> Map {
    let mut m = Map::new();

    for (y, l) in i.lines().enumerate() {
        for (x, c) in l.chars().enumerate() {
            if c == '.' { continue }

            m.insert(Coord{x: x as isize, y: y as isize}, c);
        }
    }

    m
}

fn solve_p1(i: &str) -> usize {
    let m = parse_input(i);
    let start_p = *m.iter().filter(|(_,v)| **v == 'S').next().expect("has start").0;

    #[derive(Copy, Clone)]
    struct S {
        ix: isize,
        depth: usize,
    }
    let mut visited: HashMap<Coord, S> = HashMap::new();
    let mut q: VecDeque<(Coord, S)> = VecDeque::new();

    visited.insert(start_p, S{ix: -1, depth: 0});
    for (ix, np) in get_neighbors(&start_p, &m).into_iter().enumerate() {
        q.push_back((np, S{ix: ix as isize, depth: 1}));
    }

    while let Some((p, s)) = q.pop_front() {
        let prev = visited.insert(p, s);
        if let Some(v) = prev {
            // start pos
            if p == start_p { continue }
            // same thread, already visited
            if v.ix == s.ix { continue }
            // Found solution.
            if v.ix != s.ix { return v.depth }
        }

        for np in get_neighbors(&p, &m) {
            q.push_back((np, S{ix: s.ix, depth: s.depth + 1}));
        }
    }

    panic!("No solutions?")
}

fn solve_p2(i: &str) -> usize {
    let m = parse_input(i);
    let start_p = *m.iter().filter(|(_,v)| **v == 'S').next().expect("has start").0;

    #[derive(Copy, Clone)]
    struct S {
        ix: isize,
        depth: usize,
    }
    let mut visited: HashMap<Coord, S> = HashMap::new();
    let mut q: VecDeque<(Coord, S)> = VecDeque::new();

    let mut t1: isize = 0;
    let mut t2: isize = 0;

    visited.insert(start_p, S{ix: -1, depth: 0});
    for (ix, np) in get_neighbors(&start_p, &m).into_iter().enumerate() {
        q.push_back((np, S{ix: ix as isize, depth: 1}));
    }

    while let Some((p, s)) = q.pop_front() {
        let prev = visited.insert(p, s);
        if let Some(v) = prev {
            // start pos
            if p == start_p { continue }
            // same thread, already visited
            if v.ix == s.ix { continue }
            // found solution:
            if v.ix != s.ix {
                t1 = v.ix;
                t2 = s.ix;
                break
            }
        }

        for np in get_neighbors(&p, &m) {
            q.push_back((np, S{ix: s.ix, depth: s.depth + 1}));
        }
    }

    // filter map and leave only relevant pieces
    let mut filtered_m: Map = Map::new();
    for (p, c) in m.into_iter() {
        if p == start_p || visited.get(&p).map_or(false, |s| s.ix == t1 || s.ix == t2) {
            filtered_m.insert(p, c);
        }
    }

    // Restore 'S' template based on present neighbours
    let ndv: Vec<Coord> = get_neighbors(&start_p, &filtered_m)
        .into_iter()
        .map(|c| Coord{x: c.x - start_p.x, y: c.y - start_p.y})
        .collect();
    let sc = match ndv.as_slice() {
        [Coord{x: 1, y: 0}, Coord{x: 0 , y: 1}] => 'F',
        [Coord{x: 1, y: 0}, Coord{x: 0 , y:-1}] => 'L',
        [Coord{x:-1, y: 0}, Coord{x: 0 , y: 1}] => '7',
        // no other examples
        _ => panic!("Unhandled start pointe template: {:?}", ndv),
    };
    filtered_m.insert(start_p, sc);

    // scan through values and detect their affinity
    let min_x = filtered_m.keys().map(|c| c.x).min().expect("at least one");
    let max_x = filtered_m.keys().map(|c| c.x).max().expect("at least one");
    let min_y = filtered_m.keys().map(|c| c.y).min().expect("at least one");
    let max_y = filtered_m.keys().map(|c| c.y).max().expect("at least one");

    let mut r = 0;
    for y in min_y..=max_y {
        let mut is_inside_upper = false;
        let mut is_inside_lower = false;
        for x in min_x..=max_x {
            let c = *filtered_m.get(&Coord{x, y}).unwrap_or(&'.');
            if c == '.' {
                if is_inside_upper && is_inside_lower {
                    r += 1;
                }
                continue
            }

            (is_inside_upper, is_inside_lower) = match (is_inside_upper, is_inside_lower, c) {
                // enter:
                //   expand
                (true,  false, '7') => (true,  true),
                (false, true,  'J') => (true,  true),
                //   contract
                (false, false, 'F') => (false, true),
                (false, false, 'L') => (true,  false),
                //   invert
                (false, false, '|') => (true,  true),
                // pass:
                (_, _, '-') => (is_inside_upper, is_inside_lower),
                // exit:
                //  expand
                (false, true,  '7') => (false, false),
                (true,  false, 'J') => (false, false),
                //  contract
                (true,  true,  'F') => (true,  false),
                (true,  true,  'L') => (false, true),
                //  invert
                (true,  true,  '|') => (false, false),
                _ => panic!("Unhandled state transition: {:?}", (is_inside_upper, is_inside_lower, c)),
            }
        }
    }

    r
}

fn main() {
    let e1 = std::fs::read_to_string("example1").expect("example1");
    let e2 = std::fs::read_to_string("example2").expect("example2");
    let i = std::fs::read_to_string("input").expect("input");
    eprintln!("P1 example1: {}", solve_p1(&e1));
    eprintln!("P1 example2: {}", solve_p1(&e2));
    eprintln!("P1    input: {}", solve_p1(&i));
    let e3 = std::fs::read_to_string("example3").expect("example3");
    let e4 = std::fs::read_to_string("example4").expect("example4");
    let e5 = std::fs::read_to_string("example5").expect("example5");
    eprintln!("P2 example3: {}", solve_p2(&e3));
    eprintln!("P2 example4: {}", solve_p2(&e4));
    eprintln!("P2 example5: {}", solve_p2(&e5));
    eprintln!("P2    input: {}", solve_p2(&i));
}
