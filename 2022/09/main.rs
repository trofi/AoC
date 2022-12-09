use std::fs::read_to_string;
use std::error::Error;
use std::collections::HashSet;
use std::cmp;

type E = Box<dyn Error>;

struct Op {
  n: usize,
  f: fn((isize, isize)) -> (isize, isize),
}

fn parse(i: &str) -> Vec<Op> {
  i.lines().map(|l| {
    let v: Vec<_> = l.split(' ').collect();

    Op{
      n: v[1].parse().unwrap(),
      f: match v[0] {
           "L" => |(x,y)| (x - 1, y),
           "R" => |(x,y)| (x + 1, y),
           "U" => |(x,y)| (x, y - 1),
           "D" => |(x,y)| (x, y + 1),
           _   => panic!("Unknown {} command", l),
         },
    }
  }).collect()
}

fn pull_tail(h: (isize, isize), t: (isize, isize)) -> (isize, isize) {
  let (dx, dy) = (t.0 - h.0, t.1 - h.1);
  let mut a = cmp::max(dx.abs(), dy.abs());
  a = if a == 0 { 1 } else { a };
  let dtx = dx / a;
  let dty = dy / a;

  (h.0 + dtx, h.1 + dty)
}

fn solve_p1(ops: &Vec<Op>) -> usize {
  let mut h = (0,0);
  let mut t = (0,0);
  let mut trail = HashSet::new();
  trail.insert(t);

  for op in ops {
    for _ in 0..op.n {
      h = (op.f)(h);
      t = pull_tail(h, t);
      trail.insert(t);
    }
  }

  trail.len()
}

fn solve_p2(ops: &Vec<Op>, n: usize) -> usize {
  let mut rope = Vec::new();
  rope.resize(n, (0,0));

  let mut trail: HashSet<(isize, isize)> = HashSet::new();
  trail.insert(*rope.last().unwrap());

  for op in ops {
    for _ in 0..op.n {
      rope[0] = (op.f)(rope[0]);
      for ix in 1..rope.len() {
        rope[ix] = pull_tail(rope[ix-1], rope[ix]);
      }
      trail.insert(*rope.last().unwrap());
    }
  }

  trail.len()
}

fn main() -> Result<(), E> {
  for ifile in ["example" , "example2", "input"] {
    let input = read_to_string(ifile)?;

    let cmds = parse(&input);

    let ans_p1 = solve_p1(&cmds);
    println!("{}: P1 ans: {:?}", ifile, ans_p1);
    let ans_p2 = solve_p2(&cmds, 10);
    println!("{}: P2 ans: {:?}", ifile, ans_p2);
  }
  Ok(())
}
