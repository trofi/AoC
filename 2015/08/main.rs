use std::fs::read_to_string;

fn unescape_len(s: &[char]) -> usize {
  assert!(s.first() == Some(&'"'));
  assert!(s.last() == Some(&'"'));
  let mut ms = &s[1..s.len()-1];
  let mut r = 0;

  while ms.len() > 0 {
    if ms.len() > 1 {
      if &ms[0..2] == &['\\', '"']  { ms = &ms[2..]; r += 1; continue }
      if &ms[0..2] == &['\\', '\\'] { ms = &ms[2..]; r += 1; continue }
      if &ms[0..2] == &['\\', 'x']  { ms = &ms[4..]; r += 1; continue }
    }

    ms = &ms[1..];
    r += 1;
  }

  r
}

fn escape_len(s: &[char]) -> usize {
  s.iter().map(|c|{
    match c {
      '\\' => 2,
      '"'  => 2,
      _ => 1
    }
  }).sum::<usize>() + 2
}

fn solve_p1(i: &str) -> usize {
  i.lines().map(|l|{
    let cv: Vec<char> = l.chars().collect();
    cv.len() - unescape_len(cv.as_slice())
  }).sum()
}

fn solve_p2(i: &str) -> usize {
  i.lines().map(|l|{
    let cv: Vec<char> = l.chars().collect();
     escape_len(cv.as_slice()) - cv.len()
  }).sum()
}

fn main() {
  for input_file in ["example", "input"] {
    let i = read_to_string(input_file).expect("all ok");

    let ans_p1 = solve_p1(&i);
    println!("{}: P1 ans: {:?}", input_file, ans_p1);

    let ans_p2 = solve_p2(&i);
    println!("{}: P2 ans: {:?}", input_file, ans_p2);
  }
}
