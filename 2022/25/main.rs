use std::fs::read_to_string;
use std::iter::FromIterator;

fn c2i(c: char) -> isize {
  match c {
    '0' => 0,
    '1' => 1,
    '2' => 2,
    '-' => -1,
    '=' => -2,
    _   => panic!("Unhandled {:?}", c)
  }
}

fn i2c(i: isize) -> char {
  match i {
    0 => '0',
    1 => '1',
    2 => '2',
    -1 => '-',
    -2 => '=',
    _   => panic!("Unhandled {:?}", i)
  }
}

fn s2i(s: &str) -> isize {
  s.chars().fold(0isize, |s, c| 5 * s + c2i(c))
}

fn i2s(i: isize) -> String {
  if i == 0 { return String::from("0") }

  let mut v: Vec<char> = Vec::new();
  let mut t = i;

  while t != 0 {
    let d = t % 5;
    let (borrow, c) = match d {
      0 | 1 | 2 => (0, i2c(d)),
      _         => (1, i2c(d - 5)),
    };
    v.push(c);
    t = t / 5 + borrow;
  }

  String::from_iter(v.iter().rev())
}

fn solve_p1(i: &str) -> String {
  i2s(i.lines().map(|l| s2i(l)).sum())
}

fn main() {
  for input_file in ["example", "input"] {
    let input = read_to_string(input_file).expect("input");

    println!("{}: ans: {}", input_file, solve_p1(&input));
  }
}
