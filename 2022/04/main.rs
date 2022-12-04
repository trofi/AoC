use std::cmp::min;
use std::cmp::max;
use std::fs::read_to_string;
use std::str::FromStr;
use std::error::Error;

type E = Box<dyn Error>;
fn mk_e(err: String) -> E {
  Box::<dyn Error>::from(err)
}

#[derive(Debug, Clone, PartialEq)]
struct Assignment { from: usize, to: usize, }

impl Assignment {
  fn is_empty(self: &Self) -> bool {
    self.from > self.to
  }
  fn intersect(self: &Self, other: &Self) -> Self {
    Assignment{
      from: max(self.from, other.from),
      to:   min(self.to,   other.to),
    }
  }
}

impl FromStr for Assignment {
  type Err = E;
  fn from_str(s: &str) -> Result<Self, Self::Err> {
    let numbers: Result<Vec<usize>, Self::Err> =
      s.split('-').map(|e|
        e.parse().map_err(|err| mk_e(format!("'{}': {}", e, err)))
      ).collect();
    match numbers?.as_slice() {
      &[a, b] => Ok(Assignment{from: a, to: b}),
      _       => Err(mk_e(format!("'{}' does not match N-M format", s)))
    }
  }
}

#[derive(Debug)]
struct Pair {
  segments: Vec<Assignment>,
}

impl FromStr for Pair {
  type Err = E;
  fn from_str(s: &str) -> Result<Self, Self::Err> {
    let segs: Result<Vec<Assignment>, Self::Err> = s.split(',').map(|seg|
      seg.parse().map_err(|e: <Assignment as FromStr>::Err| e.into())
    ).collect();
    Ok(Pair{segments: segs?})
  }
}

fn solve_p1(input: &Vec<&str>) -> Result<usize, E> {
  let r: Result<Vec<usize>, E> = input.iter().map(|l| {
      let p: Pair = l.parse()?;
      let i = p.segments
               .iter()
               .cloned()
               .reduce(|a, v| a.intersect(&v))
               .map_or(Err(mk_e(format!("{} ls empty", l))), Ok)?;

      Ok(if p.segments.contains(&i) { 1 } else { 0 })
    }
  ).collect();
  Ok(r?.into_iter().sum())
}

fn solve_p2(input: &Vec<&str>) -> Result<usize, E> {
  let r: Result<Vec<usize>, E> =  input.iter().map(|l| {
      let p: Pair = l.parse()?;
      let i = p.segments
               .iter()
               .cloned()
               .reduce(|a, v| a.intersect(&v))
               .map_or(Err(mk_e(format!("{} ls empty", l))), Ok)?;

      Ok(if i.is_empty() { 0 } else { 1 })
    }
  ).collect();
  Ok(r?.into_iter().sum())
}

fn main() -> Result<(), E> {
  for ifile in ["example" , "input"] {
    let input_string = read_to_string(ifile)?;
    let input: Vec<&str> = input_string.lines().collect();

    let ans_p1 = solve_p1(&input)?;
    println!("{}: P1 ans: {:?}", ifile, ans_p1);
    let ans_p2 = solve_p2(&input)?;
    println!("{}: P2 ans: {:?}", ifile, ans_p2);
  }
  Ok(())
}
