use std::fs::read_to_string;
use std::collections::BinaryHeap;
use std::collections::HashMap;
use std::collections::HashSet;

#[derive(PartialEq, Eq, Hash, Ord, PartialOrd, Clone, Copy)]
enum SquareType {
  End,
  Begin,
  Other,
}

#[derive(PartialEq, Eq, Hash, Ord, PartialOrd, Clone, Copy)]
struct Square {
  h: isize, /* normalized */
  t: SquareType,
}

fn to_h(c: char) -> isize {
  c.to_digit(36).expect("digit") as isize
}

impl Square {
  fn from_char(c: char) -> Self {
    match c {
      'S'       => Square{h: to_h('a'), t: SquareType::Begin},
      'E'       => Square{h: to_h('z'), t: SquareType::End},
      'a'..='z' => Square{h:   to_h(c), t: SquareType::Other},
      _         => panic!("{} is an unexpected square type", c),
    }
  }
}

type Pos = (isize, isize);

type Map = HashMap<Pos, Square>;

fn parse(i: &str) -> Map {
  i.lines().enumerate().map(|(row, l)|
    l.chars().enumerate().map(move |(col, ch)|
      ((row as isize, col as isize), Square::from_char(ch))
    )
  ).flatten().collect()
}

fn solve_p1(m: &Map) -> isize {
  let begin_sq =
      m.iter()
       .find(|(_, v)| v.t == SquareType::Begin)
       .expect("has begin");

  let mut q: BinaryHeap<(isize, (Pos, Square))> = BinaryHeap::new();
  q.push((-0, (*begin_sq.0, *begin_sq.1)));

  let mut visited: HashSet<Pos> = HashSet::new();

  while !q.is_empty() {
    let (steps, (p, sq)) = q.pop().expect("non empty");

    if visited.contains(&p) { continue }
    visited.insert(p);

    if sq.t == SquareType::End { return -steps }

    for np in [(p.0, p.1 - 1), (p.0, p.1 + 1), (p.0 - 1, p.1), (p.0 + 1, p.1)] {
      match m.get(&np) {
        None => (),
        Some(nsq) => {
          if nsq.h - sq.h <= 1 {
            q.push((steps - 1, (np, *nsq)));
          }
        },
      }
    }
  }

  -1
}

fn solve_p2(m: &Map) -> isize {
  let begin_sq =
      m.iter()
       .find(|(_, v)| v.t == SquareType::End)
       .expect("has end");

  let mut q: BinaryHeap<(isize, (Pos, Square))> = BinaryHeap::new();
  q.push((-0, (*begin_sq.0, *begin_sq.1)));

  let mut visited: HashSet<Pos> = HashSet::new();

  while !q.is_empty() {
    let (steps, (p, sq)) = q.pop().expect("non empty");

    if visited.contains(&p) { continue }
    visited.insert(p);

    if sq.h == to_h('a') { return -steps }

    for np in [(p.0, p.1 - 1), (p.0, p.1 + 1), (p.0 - 1, p.1), (p.0 + 1, p.1)] {
      match m.get(&np) {
        None => (),
        Some(nsq) => {
          if nsq.h - sq.h >= -1 {
            q.push((steps - 1, (np, *nsq)));
          }
        },
      }
    }
  }

  -1
}

fn main() {
  for input_file in ["example" , "input"] {
    let input = read_to_string(input_file).expect("input");

    let m = parse(&input);

    let ans_p1 = solve_p1(&m);
    println!("{}: P1 ans: {:?}", input_file, ans_p1);

    let ans_p2 = solve_p2(&m);
    println!("{}: P2 ans: {:?}", input_file, ans_p2);
  }
}
