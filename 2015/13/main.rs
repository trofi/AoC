use std::fs::read_to_string;
use std::collections::BTreeSet;
use std::collections::BTreeMap;
use std::collections::VecDeque;

fn parse<'a>(s: &'a str) -> BTreeMap<(&'a str, &'a str), isize> {
  s.lines().map(|l| {
    let v: Vec<_> = l.split(&[' ', '.']).collect();
    match v.as_slice() {
      [ name, "would", "gain", sv, "happiness", "units", "by", "sitting", "next", "to", nei, ""] =>
        ((*name, *nei), sv.parse::<isize>().expect("number")),
      [ name, "would", "lose", sv, "happiness", "units", "by", "sitting", "next", "to", nei, ""] =>
        ((*name, *nei), -sv.parse::<isize>().expect("number")),
      _ => panic!("Unhandled {:?} split", v)
    }
  }).collect()
}

fn add_self<'a>(i: &BTreeMap<(&'a str, &'a str), isize>, name: &'a str) -> BTreeMap<(&'a str, &'a str), isize> {
  let everyone: BTreeSet<&str> = i.keys().map(|(n, _)| *n).collect();
  let mut r = i.clone();
  for n in &everyone {
    r.insert((name, n), 0);
    r.insert((n, name), 0);
  }
  r
}

#[derive(PartialOrd, Ord, PartialEq, Eq, Clone)]
struct State<'a> {
  h: isize,
  head: &'a str,
  curr: &'a str,
  ordered: BTreeSet<&'a str>,
}

fn solve(i: &BTreeMap<(&str, &str), isize>) -> isize {
  let mut q: VecDeque<State> = VecDeque::new();
  let everyone: BTreeSet<&str> = i.keys().map(|(n, _)| *n).collect();

  for h in &everyone {
    for c in &everyone {
      if h == c { continue }

      let s = State {
        h: *i.get(&(h, c)).expect("value") + *i.get(&(c, h)).expect("value"),
        head: h,
        curr: c,
        ordered: BTreeSet::from([*h, *c]),
      };
      q.push_back(s);
    }
  }

  let mut best = isize::MIN;

  while !q.is_empty() {
    let s = q.pop_front().unwrap();

    if s.ordered.len() == everyone.len() {
      if s.h > best {
        best = s.h
      }
    }

    // consider new neghbor for curr
    for n in &everyone {
      if s.ordered.contains(n) { continue }

      let mut ordered = s.ordered.clone();
      ordered.insert(n);

      let mut ns = State {
        h: s.h + *i.get(&(n, s.curr)).expect("value") + *i.get(&(s.curr, n)).expect("value"),
        head: s.head,
        curr: n,
        ordered,
      };
      if ns.ordered.len() == everyone.len() {
        ns.h += *i.get(&(s.head, n)).expect("value") + *i.get(&(n, s.head)).expect("value");
      }
      q.push_back(ns);
    }
  }

  best
}

fn main() {
  for input_file in ["example", "input"] {
    let i = read_to_string(input_file).expect("all ok");

    let t = parse(&i);
    let t2 = add_self(&t, "self");

    println!("{}: P1 ans: {:?}", input_file, solve(&t));
    println!("{}: P2 ans: {:?}", input_file, solve(&t2));
  }
}
