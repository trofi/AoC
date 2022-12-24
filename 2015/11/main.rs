use std::iter::FromIterator;
use std::collections::HashSet;

fn incr(s: &[char]) -> Vec<char> {
  let mut v: Vec<char> = s.to_vec();

  for ix in (0..v.len()).rev() {
    if v[ix] < 'z' {
      v[ix] = char::from_u32(v[ix] as u32 + 1).expect("valid");
      break
    }
    v[ix] = 'a';
  }

  v
}

fn conforms(s: &[char]) -> Option<&'static str> {
  // req 1
  let mut has3: bool = false;
  for w in s.windows(3) {
    if (w[1] as i32) - (w[0] as i32) != 1 { continue }
    if (w[2] as i32) - (w[1] as i32) != 1 { continue }

    has3 = true;
    break;
  }
  if !has3 { return Some("No 3+-in-seq") }

  // req 2
  if s.contains(&'i') { return Some("Has 'i'") }
  if s.contains(&'o') { return Some("Has 'o'") }
  if s.contains(&'l') { return Some("Has 'l'") }

  // req 3
  let mut seen2: HashSet<&[char]> = HashSet::new();
  for w in s.windows(2) {
    if w[0] == w[1] {
      seen2.insert(w);
    }
  }
  if seen2.len() < 2 { return Some("No 2-dupes") }

  return None
}

fn solve(s: &str) -> String {
  let mut v: Vec<char> = s.chars().collect();

  loop {
    v = incr(&v);
    match conforms(&v) {
      None => break,
      Some(_) => {}, //println!("Candidate {} failed at {}", String::from_iter(&v), err)
    }
  }

  String::from_iter(&v)
}

fn main() {
  // vzbxtuvz: wrong
  println!("abcdefgh ans: {:?}", solve("abcdefgh"));
  println!("ghijklmn ans: {:?}", solve("ghijklmn"));
  println!("P1 ans: {:?}", solve("vzbxkghb"));
  println!("P2 ans: {:?}", solve(solve("vzbxkghb").as_str()));
}
