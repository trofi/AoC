use std::collections::HashSet;
use std::collections::BinaryHeap;

struct Input<'a> {
  rules: Vec<(&'a [u8], &'a [u8])>,
  s: &'a [u8],
}

fn parse(i: &str) -> Input {
  let v: Vec<_> = i.split("\n\n").collect();
  let (srules, s) = match v.as_slice() {
    [srules, s] => (srules, s.trim().as_bytes()),
    _ => panic!("Unexpected newline count, wanted one double, got: {}", v.len())
  };

  let rules = srules.lines().map(|l| {
    let vr: Vec<_> = l.split(" => ").collect();
    match vr.as_slice() {
      [k, v] => (k.as_bytes(), v.as_bytes()),
      _ => panic!("Unhandled rule {:?}", vr)
    }
  }).collect();

  Input { rules, s }
}

fn solve_p1(i: &Input) -> usize {
  let mut r: HashSet<Vec<u8>> = HashSet::new();

  for (k, v) in &i.rules {
    for (ix, w) in i.s.windows(k.len()).enumerate() {
      if *k != w { continue }

      let mut rs: Vec<u8> = Vec::new();
      for c in &i.s[0..ix]        { rs.push(*c); }
      for c in *v                 { rs.push(*c); }
      for c in &i.s[ix+k.len()..] { rs.push(*c); }

      r.insert(rs);
    }
  }

  r.len()
}

fn solve_p2(i: &Input) -> usize {
  let mut q: BinaryHeap<(isize, isize, Vec<u8>)> = BinaryHeap::new();
  // Our priority is: -(round+string_len).
  q.push((0 - (i.s.len() as isize), 0, i.s.to_vec()));

  let mut visited: HashSet<(isize, Vec<u8>)> = HashSet::new();

  let mut mr = isize::MAX;

  while !q.is_empty() {
    let (_, r, s) = q.pop().unwrap();

    if r >= mr { continue }

    if visited.contains(&(r, s.clone())) { continue }
    visited.insert((r, s.clone()));

    if s == [b'e'] {
      mr = std::cmp::min(mr, r);
      continue
    }

    for (k, v) in &i.rules {
      for (ix, w) in s.windows(v.len()).enumerate() {
        if *v != w { continue }

        let mut rs: Vec<u8> = Vec::new();
        for c in &s[0..ix]        { rs.push(*c); }
        for c in *k               { rs.push(*c); }
        for c in &s[ix+v.len()..] { rs.push(*c); }

        q.push((r - 1 - (rs.len() as isize), r - 1, rs));
      }
    }
  }

  (-mr) as usize
}

fn main() {
  for input_file in ["example", "input"] {
    let i = std::fs::read_to_string(input_file).expect("all ok");

    let input = parse(&i);

    println!("{}: P1 ans: {:?}", input_file, solve_p1(&input));
    println!("{}: P2 ans: {:?}", input_file, solve_p2(&input));
  }
}
