use std::fs::read_to_string;
use std::collections::BTreeMap;
use std::collections::BTreeSet;
use std::collections::BinaryHeap;

type Pos = (isize, isize);

type Ground = BTreeMap<Pos, char>;
type Blizzard = BTreeMap<Pos, Vec<char>>;

#[derive(Ord, PartialOrd, Eq, PartialEq, Clone)]
struct Field { ground: Ground, blizzards: Blizzard }

fn u2p(x: usize, y: usize) -> Pos { (x as isize, y as isize) }

fn parse(i: &str) -> Field {
  let mut ground: Ground = Ground::new();
  let mut blizzards: Blizzard = Blizzard::new();

  for (y, l) in i.lines().enumerate() {
    for (x, v) in l.chars().enumerate() {
      match v {
        '#' | '.' => { ground.insert(u2p(x, y), v); }
        '<' | '>' | '^' | 'v' => {
          ground.insert(u2p(x, y), '.');
          blizzards.insert(u2p(x, y), vec![v]);
        }
        _ => unreachable!()
      }
    }
  }

  Field { ground, blizzards }
}

fn tick(b: &Blizzard, lu: Pos, rd: Pos) -> Blizzard {
  let mut r: Blizzard = Blizzard::new();

  for ((x, y), vbd) in b {
    for d in vbd {
      let nbp = match d {
        '>' => if (*x + 1) != rd.0 { (*x + 1, *y) } else { (lu.0 + 1, *y) }
        'v' => if (*y + 1) != rd.1 { (*x, *y + 1) } else { (*x, lu.1 + 1) }
        '<' => if (*x - 1) != lu.0 { (*x - 1, *y) } else { (rd.0 - 1, *y) }
        '^' => if (*y - 1) != lu.1 { (*x, *y - 1) } else { (*x, rd.1 - 1) }
        _ => unreachable!()
      };
      match r.get_mut(&nbp) {
        None => { r.insert(nbp, vec![*d]); },
        Some(v) => { v.push(*d); }
      }
    }
  }

  r
}

#[derive( Ord, PartialOrd, Eq, PartialEq, Clone)]
struct State { pos: Pos, time: usize }

fn reach(f: &Field, from: Pos, to: Pos, start_time: usize) -> usize {
  let mut all_blizzard_pats: Vec<Blizzard> = Vec::new();
  {
    let mut b = f.blizzards.clone();
    let (lu, rd) = if from < to { (from, to) } else { (to, from) };
    loop {
      all_blizzard_pats.push(b.clone());
      b = tick(&b, (lu.0 - 1, lu.1), (rd.0 + 1, rd.1));
      if b == f.blizzards { break }
    }
  }

  let mut q: BinaryHeap<(isize, State)> = BinaryHeap::new();
  let mut visited: BTreeSet<State> = BTreeSet::new();

  let prio = |pos: Pos, time: usize| -> isize {
    - (mdist(&to, &pos) + (time as isize))
  };

  let is = State { pos: from, time: start_time };
  q.push((prio(is.pos, is.time), is));

  let mut t: usize = usize::MAX;

  while !q.is_empty() {
    let (_, s) = q.pop().unwrap();
    if visited.contains(&s) { continue }
    visited.insert(s.clone());

    if s.pos == to {
      if s.time < t {
        t = s.time;
      }
      continue
    }

    let time = s.time + 1;
    let b = &all_blizzard_pats[time % all_blizzard_pats.len()];

    let (x, y) = s.pos;
    for pos in [(x, y), (x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)] {
      if f.ground.get(&pos) != Some (&'.') { continue }
      if b.get(&pos).is_some() { continue }

      // can improve?
      if t > time + (mdist(&to, &pos) as usize) {
        let ns = State { pos, time };
        q.push((prio(ns.pos, ns.time), ns));
      }
    }
  }

  t
}

fn mdist((x0, y0): &Pos, (x1, y1): &Pos) -> isize {
  (x1 - x0).abs() + (y1 - y0).abs() as isize
}

fn solve_p1(f: &Field) -> usize {
  let walkable = f.ground.iter().filter(|(_,v)| **v == '.').map(|(p,_)| p);
  let st: Pos = *walkable.clone().min().unwrap();
  let en: Pos = *walkable        .max().unwrap();

  reach(f, st, en, 0)
}

fn solve_p2(f: &Field) -> usize {
  let walkable = f.ground.iter().filter(|(_,v)| **v == '.').map(|(p,_)| p);
  let st: Pos = *walkable.clone().min().unwrap();
  let en: Pos = *walkable        .max().unwrap();

  let t1 = reach(f, st, en, 0);
  let t2 = reach(f, en, st, t1);
           reach(f, st, en, t2)
}

fn main() {
  for input_file in ["example", "input"] {
    let input = read_to_string(input_file).expect("input");

    let i = parse(&input);

    println!("{}: P1 ans: {:?}", input_file, solve_p1(&i));
    println!("{}: P2 ans: {:?}", input_file, solve_p2(&i));
  }
}
