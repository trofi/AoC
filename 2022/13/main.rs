use std::cmp::Ordering;
use std::fs::read_to_string;
use std::iter::zip;

#[derive(PartialEq, Eq, Clone, Debug)]
enum Val {
  I(isize),
  L(Vec<Val>),
}

impl Ord for Val {
  fn cmp(&self, other: &Self) -> Ordering {
    match (self, other) {
      (Val::I(li), Val::I(ri)) => li.cmp(ri),
      (Val::I(_), Val::L(_))   => Val::L(Vec::from([self.clone()])).cmp(other),
      (Val::L(_), Val::I(_))   => self.cmp(&Val::L(Vec::from([other.clone()]))),
      (Val::L(ll), Val::L(rl)) => {
        for (l, r) in zip(ll, rl) {
          let c = l.cmp(r);
          if c != Ordering::Equal { return c }
        }
        ll.len().cmp(&rl.len())
      },
    }
  }
}

impl PartialOrd for Val {
    fn partial_cmp(&self, other: &Val) -> Option<Ordering> {
      Some(self.cmp(other))
    }
}

fn parse_int(i: &[char]) -> (Val, &[char]) {
  let mut r = 0;
  let mut ix = 0;

  loop {
    assert!(ix < i.len());
    if !i[ix].is_digit(10) { break }

    let d = i[ix].to_digit(10).expect("decimal") as isize;
    r = 10 * r + d;

    ix += 1;
  }

  assert!(ix > 0);
  (Val::I(r), &i[ix..])
}

fn parse_list(i: &[char]) -> (Val, &[char]) {
  assert!(i[0] == '[');
  let mut p = &i[1..];

  let mut v = Vec::new();
  loop {
    if p[0] == ']' { break }

    let val: Val;
    (val, p) = parse_val(p);
    v.push(val);

    if p[0] != ',' { break }
    p = &p[1..];
  }

  assert!(p[0] == ']');
  (Val::L(v), &p[1..])
}

fn parse_val(i: &[char]) -> (Val, &[char]) {
  match i[0] {
    '['=> parse_list(i),
    _  => parse_int(i),
  }
}

fn parse(i: &str) -> Vec<Vec<Val>>{
  i.split("\n\n").map(|lpairs|
    lpairs.lines().map(|l|{
      let vc: Vec<char> = l.chars().collect();
      let (v, rest) = parse_val(&vc);
      assert!(rest.len() == 0); /* parsed everything */
      v
    }).collect()
  ).collect()
}

fn solve_p1(i: &Vec<Vec<Val>>) -> usize {
  i.iter()
   .enumerate()
   .filter(|(_, p)| { p[0] <= p[1]})
   .map(|(i, _)| i + 1)
   .sum()
}

fn solve_p2(i: &Vec<Vec<Val>>) -> usize {
  let mut v: Vec<&Val> = i.iter().flatten().collect();
  let d1 = parse_val(&['[', '[', '2', ']', ']']).0;
  let d2 = parse_val(&['[', '[', '6', ']', ']']).0;
  v.push(&d1);
  v.push(&d2);
  v.sort();

  [&d1, &d2].iter()
            .map(|e| v.iter()
                      .position(|v| v == e)
                      .unwrap() + 1)
            .product()
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
