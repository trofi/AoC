use std::fs::read_to_string;
use std::collections::HashSet;

fn solve_p1(i: &str) -> usize {
  let mut pos = (0,0);
  let mut visited = HashSet::new();
  visited.insert(pos);

  for a in i.chars() {
    match a {
      '>' => pos.0 += 1,
      '<' => pos.0 -= 1,
      'v' => pos.1 += 1,
      '^' => pos.1 -= 1,
      _   => panic!("Unknown {} command", a),
    }

    visited.insert(pos);
  }

  visited.len()
}

fn solve_p2(i: &str) -> usize {
  let mut pos = vec![(0,0); 2];
  let mut ix = 0;
  let mut visited = HashSet::new();
  visited.insert(pos[ix]);

  for a in i.chars() {
    match a {
      '>' => pos[ix].0 += 1,
      '<' => pos[ix].0 -= 1,
      'v' => pos[ix].1 += 1,
      '^' => pos[ix].1 -= 1,
      _   => panic!("Unknown {} command", a),
    }

    visited.insert(pos[ix]);
    ix = (ix + 1) % pos.len();
  }

  visited.len()
}

fn main() {
  for input_file in ["example", "input"] {
    let i = read_to_string(input_file).expect("all ok");

    let ans_p1 = solve_p1(&i);
    println!("P1 ans: {:?}", ans_p1);
    let ans_p2 = solve_p2(&i);
    println!("P2 ans: {:?}", ans_p2);
  }
}
