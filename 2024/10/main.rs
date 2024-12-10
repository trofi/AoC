use std::collections::HashMap;
use std::collections::HashSet;
use std::collections::VecDeque;

type Coord = (isize, isize);

type Map = HashMap<Coord, usize>;

fn parse_input(i: &str) -> Map {
    let mut m = Map::new();

    for (y, l) in i.lines().enumerate() {
        for (x, c) in l.chars().enumerate() {
            let v: usize = c.to_digit(10).expect("a digit") as usize;
            m.insert((x as isize, y as isize), v);
        }
    }

    m
}

fn trace_p1(sp: &Coord, m: &Map) -> usize {
    // the key is the visited coordinate
    let mut visited: HashSet<Coord> = HashSet::new();
    let mut queue: VecDeque<(Coord, usize)> = VecDeque::new();
    queue.push_back((*sp, 0));

    while let Some((p, v)) = queue.pop_front() {
        if visited.contains(&p) { continue }
        visited.insert(p);

        // explore neighbors with v+1 height
        for np in [(p.0 - 1, p.1), (p.0 + 1, p.1), (p.0, p.1 - 1), (p.0, p.1 + 1)] {
            if let Some(nv) = m.get(&np) {
                if *nv == v + 1 {
                    queue.push_back((np, *nv));
                }
            }
        }
    }

    // leave only trails that finish at '9'
    visited.into_iter()
           .filter(|e| m.get(e) == Some(&9))
           .count()
}

fn trace_p2(sp: &Coord, m: &Map) -> usize {
    // the key is the full visited path
    let mut visited: HashSet<Vec<Coord>> = HashSet::new();
    let mut queue: VecDeque<(Vec<Coord>, usize)> = VecDeque::new();
    queue.push_back((vec![*sp], 0));

    while let Some((vp, v)) = queue.pop_front() {
        if visited.contains(&vp) { continue }
        visited.insert(vp.clone());

        assert!(vp.len() > 0);
        let p = vp[vp.len()-1];
        // explore neighbors with v+1 height
        for np in [(p.0 - 1, p.1), (p.0 + 1, p.1), (p.0, p.1 - 1), (p.0, p.1 + 1)] {
            if let Some(nv) = m.get(&np) {
                if *nv == v + 1 {
                    let mut nvp: Vec<Coord> = vp.clone();
                    nvp.push(np);
                    queue.push_back((nvp, *nv));
                }
            }
        }
    }

    // leave only trails that finish at '9'
    visited.into_iter()
           .filter(|nvp| m.get(&nvp[nvp.len() - 1]) == Some(&9))
           .count()
}

fn solve(i: &str, f: fn(&Coord, &Map) -> usize) -> usize {
    let m = parse_input(i);

    m.iter()
      .filter(|(_,c)| *c == &0)
      .map(|(p,_)| f(p, &m))
      .sum()
}

fn main() {
    let e = std::fs::read_to_string("example").expect("example");
    let i = std::fs::read_to_string("input").expect("input");
    println!("P1 e: {:?}", solve(&e, trace_p1));
    println!("P1 i: {:?}", solve(&i, trace_p1));
    println!("P2 e: {:?}", solve(&e, trace_p2));
    println!("P2 i: {:?}", solve(&i, trace_p2));
}
