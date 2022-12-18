use std::fs::read_to_string;
use std::collections::BTreeSet;
use std::collections::VecDeque;

type P = (isize, isize, isize);

fn parse(i: &str) -> BTreeSet<P> {
  i.lines().map(|l|{
    let v : Vec<_> = l.split(',').map(|e| e.parse().expect("number")).collect();
    match v.as_slice() {
      &[x, y, z] => (x,y,z),
      _ => panic!("Unexpected coord count in {:?},expected 3", v)
    }
  }).collect()
}

// p1 bits:

fn solve_p1(i: &BTreeSet<P>) -> usize {
  // inspect all neighboring cubes' surfaces.
  // Count only surfaces that don't have a neighbor cube.
  i.iter().cloned().map(|(x,y,z)|
    [(x - 1, y, z), (x + 1, y, z),
     (x, y - 1, z), (x, y + 1, z),
     (x, y, z - 1), (x, y, z + 1)]
        .iter()
        .filter(|p| !i.contains(p))
        .count()
  ).sum()
}

// p2 bits:

fn bounding_box(i: &BTreeSet<P>) -> (P, P) {
  let mi = (
    i.iter().map(|(x,_,_)| x).min().unwrap() - 1,
    i.iter().map(|(_,y,_)| y).min().unwrap() - 1,
    i.iter().map(|(_,_,z)| z).min().unwrap() - 1
  );
  let ma = (
    i.iter().map(|(x,_,_)| x).max().unwrap() + 1,
    i.iter().map(|(_,y,_)| y).max().unwrap() + 1,
    i.iter().map(|(_,_,z)| z).max().unwrap() + 1
  );

  (mi, ma)
}

fn surface(lx: isize, ly: isize, lz: isize) -> usize {
  2 * (lx * ly + ly * lz + lx * lz) as usize
}

fn solve_p2(i: &BTreeSet<P>) -> usize {
  // calculate shape of between bounding box and our shape
  // Then use P1 solution to get the surface and subtract
  // exterior surface of a cuboid

  let (mi, ma) = bounding_box(i);

  let mut visited: BTreeSet<P> = BTreeSet::new();
  let mut q: VecDeque<P> = VecDeque::new();

  q.push_back(mi);
  while !q.is_empty() {
    let c@(x,y,z) = q.pop_front().unwrap();
    if visited.contains(&c) { continue }
    visited.insert(c);

    for nc in [(x - 1, y, z), (x + 1, y, z),
               (x, y - 1, z), (x, y + 1, z),
               (x, y, z - 1), (x, y, z + 1)] {
      if i.contains(&nc) { continue }
      if visited.contains(&nc) { continue }
      if nc.0 < mi.0 || nc.0 > ma.0 { continue }
      if nc.1 < mi.1 || nc.1 > ma.1 { continue }
      if nc.2 < mi.2 || nc.2 > ma.2 { continue }

      q.push_back(nc);
    }
  }

  solve_p1(&visited) - surface(ma.0 - mi.0 + 1,
                               ma.1 - mi.1 + 1,
                               ma.2 - mi.2 + 1)
}

fn main() {
  for input_file in ["example", "input"] {
    let input = read_to_string(input_file).expect("input");

    let cubes = parse(&input);

    println!("{}: P1 ans: {:?}", input_file, solve_p1(&cubes));
    println!("{}: P2 ans: {:?}", input_file, solve_p2(&cubes));
  }
}
