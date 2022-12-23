use std::fs::read_to_string;
use std::collections::HashSet;
use std::collections::HashMap;

type Pos   = (isize, isize);
type Field = HashSet<Pos>;

fn parse(i: &str) -> Field {
  let mut f: Field = Field::new();

  for (r, l) in i.lines().enumerate() {
    for (c, v) in l.chars().enumerate() {
      if v == '#' {
        f.insert((r as isize, c as isize));
      }
    }
  }

  f
}

#[derive(Clone, Copy)]
enum Dir { N, S, W, E }

const DIRS: &[Dir] = &[Dir::N, Dir::S, Dir::W, Dir::E];

fn neighs((r, c): Pos, d: Dir) -> Vec<Pos> {
  match d {
    Dir::N => vec![(r - 1, c - 1), (r - 1, c), (r - 1, c + 1)],
    Dir::S => vec![(r + 1, c - 1), (r + 1, c), (r + 1, c + 1)],
    Dir::W => vec![(r - 1, c - 1), (r, c - 1), (r + 1, c - 1)],
    Dir::E => vec![(r - 1, c + 1), (r, c + 1), (r + 1, c + 1)],
  }
}

fn all_neighs((r, c): Pos) -> Vec<Pos> {
  vec![
    (r - 1, c - 1), (r - 1, c), (r - 1, c + 1),
    (r,     c - 1),             (r,     c + 1),
    (r + 1, c - 1), (r + 1, c), (r + 1, c + 1),
  ]
}

fn iter(f: &Field, i: usize) -> Field {
  let mut proposals: HashMap<Pos, Vec<Pos>> = HashMap::new();

  for p@(r, c) in f {
    for dix in 0..4 {
      let all_nps = all_neighs(*p);
      if all_nps.iter().all(|np| !f.contains(np)) {
        continue
      }

      let d = DIRS[(i + dix) % DIRS.len()];
      let nps = neighs(*p, d);

      if nps.iter().all(|np| !f.contains(np)) {
        let tp = match d {
          Dir::N => (*r - 1, *c),
          Dir::S => (*r + 1, *c),
          Dir::W => (*r, *c - 1),
          Dir::E => (*r, *c + 1),
        };

        match proposals.get_mut(&tp) {
          None => { proposals.insert(tp, vec![*p]); },
          Some(v) => v.push(*p)
        }
        break
      }
    }
  }

  let mut r: Field = f.clone();

  for (tp, srcs) in proposals {
    if srcs.len() == 1 {
      let sp = srcs[0];

      let had_sp = r.remove(&sp);
      assert!(had_sp);
      let new_tp = r.insert(tp);
      assert!(new_tp);
    }
  }

  r
}

fn solve_p1(i: &Field) -> isize {
  let mut f: Field = i.clone();

  for i in 0..10 {
    f = iter(&f, i);
  }

  let min_r = f.iter().map(|(_,r)| r).min().unwrap();
  let max_r = f.iter().map(|(_,r)| r).max().unwrap();
  let min_c = f.iter().map(|(c,_)| c).min().unwrap();
  let max_c = f.iter().map(|(c,_)| c).max().unwrap();

  (max_r - min_r + 1) * (max_c - min_c + 1) - (f.len() as isize)
}

fn solve_p2(i: &Field) -> Option<usize> {
  let mut f: Field = i.clone();

  for i in 0.. {
    // TODO: iter could do inplace changes
    // to the Field and return a hint if
    // the field was modified.
    let new_f = iter(&f, i);
    if new_f == f { return Some(i + 1) }
    f = new_f;
  }

  None
}

fn main() {
  for input_file in ["example", "input"] {
    let input = read_to_string(input_file).expect("input");

    let i = parse(&input);

    println!("{}: P1 ans: {:?}", input_file, solve_p1(&i));
    println!("{}: P2 ans: {:?}", input_file, solve_p2(&i));
  }
}
