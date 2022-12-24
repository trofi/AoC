use std::fs::read_to_string;

#[derive(Clone, Copy, Debug)]
struct Pair {
  v: char,
  n: usize,
}

fn parse(s: &str) -> Vec<Pair> {
  s.chars().map(|c| Pair{v: c, n: 1}).collect()
}

fn compress(s: &[Pair]) -> Vec<Pair> {
  let mut r: Vec<Pair> = Vec::new();

  let mut ov: Option<Pair> = None;

  for e in s {
    match ov {
      None => { ov = Some(*e); },
      Some(ref mut v) => {
        if e.v == v.v {
          v.n += e.n;
        } else {
          r.push(*v);
          ov = Some(*e);
        }
      }
    }
  }

  if let Some(v) = ov { r.push(v); }

  r
}

fn expand(s: &[Pair]) -> Vec<Pair> {
  let mut r: Vec<Pair> = Vec::new();

  for e in s {
    let s = e.n.to_string();
    for c in s.chars() {
      r.push(Pair{v: c, n: 1});
    }
    r.push(Pair{v: e.v, n: 1});
  }

  r
}

fn solve(i: &[Pair], rounds: usize) -> usize {
  let mut r: Vec<Pair> = i.to_vec();

  for _ in 0..rounds {
    r = compress(&r);
    r = expand(&r);
  }

  r.iter().map(|p| p.n).sum()
}

fn main() {
  for input_file in ["example", "input"] {
    let i = read_to_string(input_file).expect("all ok");

    let m = parse(&i);

    println!("{}: P1 ans: {:?}", input_file, solve(&m, 40));
    println!("{}: P2 ans: {:?}", input_file, solve(&m, 50));
  }
}
