use std::cmp::min;
use std::cmp::max;
use std::fs::File;
use std::io::BufRead;
use std::io::BufReader;
use std::str::FromStr;

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
  type Err = ();
  fn from_str(s: &str) -> Result<Self, Self::Err> {
    let numbers: Vec<usize> =
      s.split('-').map(|e|
        e.parse().expect("a number")
      ).collect();
    match numbers.as_slice() {
      &[a, b] => Ok(Assignment{from: a, to: b}),
      _       => Err(()),
    }
  }
}

#[derive(Debug)]
struct Pair {
  segments: Vec<Assignment>,
}

impl FromStr for Pair {
  type Err = ();
  fn from_str(s: &str) -> Result<Self, Self::Err> {
    let segs: Vec<Assignment> = s.split(',').map(|seg|
      seg.parse().expect("N-M format")
    ).collect();
    Ok(Pair{segments: segs})
  }
}

fn solve_p1(input: &Vec<String>) -> Result<usize, ()> {
  let r = input.iter().map(|l| {
      let p: Pair = l.parse().expect("pair format did not match");
      let i = p.segments
               .iter()
               .cloned()
               .reduce(|a, v| a.intersect(&v))
               .expect("at least one element");

      if p.segments.contains(&i) { 1 } else { 0 }
    }
  ).sum();
  Ok(r)
}

fn solve_p2(input: &Vec<String>) -> Result<usize, ()> {
  let r =  input.iter().map(|l| {
      let p: Pair = l.parse().expect("pair format did not match");
      let i = p.segments
               .iter()
               .cloned()
               .reduce(|a, v| a.intersect(&v))
               .expect("at least one element");

      if i.is_empty() { 0 } else { 1 }
    }
  ).sum();
  Ok(r)
}

fn read_input(input_file: &str) -> Result<Vec<String>, ()> {
  let f = File::open(input_file).unwrap();
  let bf = BufReader::new(f);

  let r = bf.lines().map(|l| l.expect("a line")).collect();
  Ok(r)
}

fn main() -> Result<(), ()> {
  for ifile in ["example" , "input"] {
    let input = read_input(ifile)?;
    let ans_p1 = solve_p1(&input)?;
    println!("{}: P1 ans: {:?}", ifile, ans_p1);
    let ans_p2 = solve_p2(&input)?;
    println!("{}: P2 ans: {:?}", ifile, ans_p2);
  }
  Ok(())
}
