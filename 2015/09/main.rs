use std::fs::read_to_string;
use std::collections::BTreeSet;
use std::collections::HashSet;
use std::collections::VecDeque;

type Link<'a> = (&'a str, &'a str, usize);

fn parse<'a>(s: &'a str) -> HashSet<Link<'a>> {
  s.lines().map(|l| {
    let vl: Vec<_> = l.split(" = ").collect();
    assert!(vl.len() == 2);
    let vsd: Vec<_> = vl[0].split(" to ").collect();
    assert!(vsd.len() == 2);
    (vsd[0], vsd[1], vl[1].parse().expect("distance"))
  }).collect()
}

fn solve_p1(i: &HashSet<Link>) -> usize {
  let mut q: VecDeque<(&str, BTreeSet<&str>, usize)> = VecDeque::new();

  let mut all_cities: HashSet<&str> = HashSet::new();

  for (s, d, l) in i {
    let mut v: BTreeSet<&str> = BTreeSet::new();
    v.insert(s);
    v.insert(d);
    q.push_back((d, v.clone(), *l));
    q.push_back((s, v, *l));

    all_cities.insert(s);
    all_cities.insert(d);
  }

  let mut r = std::usize::MAX;

  while !q.is_empty() {
    let (c, v, l) = q.pop_front().unwrap();

    if v.len() == all_cities.len() {
      if l < r {
        r = l;
      }
    }

    for (ns, nd, nl) in i {
      if ns == &c && !v.contains(nd) {
        let mut nv = v.clone();
        nv.insert(nd);
        q.push_back((nd, nv, l + nl));
      }

      if nd == &c && !v.contains(ns) {
        let mut nv = v.clone();
        nv.insert(ns);
        q.push_back((ns, nv, l + nl));
      }
    }
  }

  r
}

fn solve_p2(i: &HashSet<Link>) -> usize {
  let mut q: VecDeque<(&str, BTreeSet<&str>, usize)> = VecDeque::new();

  let mut all_cities: HashSet<&str> = HashSet::new();

  for (s, d, l) in i {
    let mut v: BTreeSet<&str> = BTreeSet::new();
    v.insert(s);
    v.insert(d);
    q.push_back((d, v.clone(), *l));
    q.push_back((s, v, *l));

    all_cities.insert(s);
    all_cities.insert(d);
  }

  let mut r = std::usize::MIN;

  while !q.is_empty() {
    let (c, v, l) = q.pop_front().unwrap();

    if v.len() == all_cities.len() {
      if l > r {
        r = l;
      }
    }

    for (ns, nd, nl) in i {
      if ns == &c && !v.contains(nd) {
        let mut nv = v.clone();
        nv.insert(nd);
        q.push_back((nd, nv, l + nl));
      }

      if nd == &c && !v.contains(ns) {
        let mut nv = v.clone();
        nv.insert(ns);
        q.push_back((ns, nv, l + nl));
      }
    }
  }

  r
}

fn main() {
  for input_file in ["example", "input"] {
    let i = read_to_string(input_file).expect("all ok");

    let m = parse(&i);

    println!("{}: P1 ans: {:?}", input_file, solve_p1(&m));
    println!("{}: P2 ans: {:?}", input_file, solve_p2(&m));
  }
}
