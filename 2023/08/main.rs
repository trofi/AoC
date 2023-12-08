use std::collections::HashMap;

struct Input<'a> {
    steps: &'a str,
    map: HashMap<&'a str, (&'a str, &'a str)>,
}

fn parse_input(i: &str) -> Input {
    let iv: Vec<&str> = i.split("\n\n").collect();
    assert!(iv.len() == 2);
    let steps = iv[0];
    let map: HashMap<&str, (&str, &str)> =
        iv[1].lines()
             .map(|l| {
                 // parse simething like "AAA = (BBB, CCC)"
                 let lv: Vec<&str> = l.split(&[' ', '=', '(', ',', ')']).collect();
                 match lv.as_slice() {
                     [s, "", "", "", l, "", r, ""] => (*s, (*l, *r)),
                     _ => panic!("Unhandled {:?} map entry", lv),
                 }
             }).collect();
    Input{steps, map}
}

fn solve_p1(i: &str) -> usize {
    let i = parse_input(i);

    let mut pos: &str = "AAA";
    for (ix, c) in i.steps.chars().cycle().enumerate() {
        if pos == "ZZZ" { return ix }

        let next = i.map.get(pos).expect("target");
        pos = match c {
            'L' => next.0,
            'R' => next.1,
            _ => panic!("Unhandled {:?} step type", c),
        };
    }
    panic!("Unreachable");
}

enum Steps {
    Next(usize), // n
    Wrap(usize, usize), // n, ix
}

impl Steps {
    fn next(self: &Self) -> usize {
        match self {
            Steps::Next(n) => *n,
            Steps::Wrap(n, _) => *n,
        }
    }
}

struct Trail {
    // count of steps to the next point
    steps: Vec<Steps>,
}

struct TrailIterator<'a> {
    t: &'a Trail,
    ix: usize,
}

impl<'a> TrailIterator<'a> {
    fn new(t: &'a Trail) -> Self {
        Self {
            t,
            ix: 0,
        }
    }
}

impl<'a> Iterator for TrailIterator<'a> {
    type Item = usize;
    fn next(&mut self) -> Option<Self::Item> {
        let v = self.t.steps[self.ix].next();

        self.ix = match self.t.steps[self.ix] {
            Steps::Next(_) => self.ix + 1,
            Steps::Wrap(_, ix) => ix,
        };

        Some(v)
    }
}

fn get_trail(pos: &str, i: &Input) -> Trail {
    let mut steps = Vec::new();
    let mut points: Vec<&str> = Vec::new();
    let mut from = pos;
    let mut from_ix = 0;

    let mut p = pos;
    for (ix, c) in i.steps.chars().cycle().enumerate() {
        if p.chars().last() == Some('Z') {
            if p == from && false {
                steps.push(Steps::Wrap(ix - from_ix, steps.len()));
                break;
            }
            if let Some(i) = points.iter().position(|e| *e == p) {
                steps.push(Steps::Wrap(ix - from_ix, i));
                break
            }
            points.push(from);
            steps.push(Steps::Next(ix - from_ix));
            from = p;
            from_ix = ix;
        }

        let next = i.map.get(p).expect("target");
        p = match c {
            'L' => next.0,
            'R' => next.1,
            _ => panic!("Unhandled {:?} step type", c),
        };
    }

    Trail{steps}
}

fn solve_p2(i: &str) -> usize {
    let i = parse_input(i);

    let trails: Vec<Trail> =
        i.map.keys()
             .filter(|k| k.chars().last() == Some ('A'))
             .map(|pos| get_trail(pos, &i))
             .collect();

    let l: usize = trails.len();

    let mut r: Vec<(usize, TrailIterator)> =
        trails.iter()
              .map(|t| (0, TrailIterator::new(t)))
              .collect();

    // Skip starting point, thus not '0'.
    let mut max_v: usize = 1;
    loop {
        let mut all_eq = true;
        for ix in 0..l {
            while r[ix].0 < max_v {
                let v = r[ix].1.next().expect("unexpected finish");
                r[ix].0 += v;
            }
            if r[ix].0 > max_v {
                max_v = r[ix].0;
                if ix > 0 {
                    all_eq = false;
                }
            }
        }

        if all_eq { break }
    }

    max_v
}

fn main() {
    let e = std::fs::read_to_string("example").expect("example");
    let e2 = std::fs::read_to_string("example2").expect("example2");
    let i = std::fs::read_to_string("input").expect("input");
    println!("P1  example: {}", solve_p1(&e));
    println!("P1 example2: {}", solve_p1(&e2));
    println!("P1    input: {}", solve_p1(&i));

    let e3 = std::fs::read_to_string("example3").expect("example3");
    println!("P2 example3: {}", solve_p2(&e3));
    println!("P2    input: {}", solve_p2(&i));
}
