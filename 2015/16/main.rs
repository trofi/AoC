use std::collections::HashMap;

fn s2i(s: &str) -> usize {
  s.parse().unwrap()
}

fn parse_facts<'a>(s: &'a str) -> Vec<HashMap<&'a str, usize>> {
  s.lines().map(|l| {
    let v: Vec<_> = l.split(&[' ', ':', ',']).collect();
    match v.as_slice() {
      ["Sue", _,
       "", k1, "", v1,
       "", k2, "", v2,
       "", k3, "", v3] => HashMap::from([(*k1, s2i(v1)), (*k2, s2i(v2)), (*k3, s2i(v3))]),
      _ => panic!("Unhandled input {:?}", v)
    }    
  }).collect()
}

fn parse_input<'a>(s: &'a str) -> HashMap<&'a str, usize> {
  s.lines().map(|l| {
    let v: Vec<_> = l.split(&[' ', ':']).collect();
    match v.as_slice() {
        [k, "", v] => (*k, s2i(v)),
      _ => panic!("Unhandled input {:?}", v)
    }
  }).collect()
}

fn solve_p1(f: &[HashMap<&str, usize>], i: &HashMap<&str, usize>) -> Vec<usize> {
  let mut r: Vec<_> = Vec::new();
  for (ix, o) in f.iter().enumerate() {
    if o.iter().all(|(k, v)| i.get(k) == Some(v)) {
      r.push(ix + 1);
    }
  }
  r
}

fn solve_p2(f: &[HashMap<&str, usize>], i: &HashMap<&str, usize>) -> Vec<usize> {
  let mut r: Vec<_> = Vec::new();
  for (ix, o) in f.iter().enumerate() {
    if o.iter().all(|(k, v)| match (k, i.get(k)) {
                        (&"cats",        Some(iv)) => v > iv,
                        (&"trees",       Some(iv)) => v > iv,
                        (&"pomeranians", Some(iv)) => v < iv,
                        (&"goldfish",    Some(iv)) => v < iv,
                        (_, Some(iv))              => v == iv,
                        _                          => true,
                   }) {
      r.push(ix + 1);
    }
  }
  r
}

fn main() {
  let sf = std::fs::read_to_string("facts").unwrap();
  let si = std::fs::read_to_string("input").unwrap();
 
  let f = parse_facts(&sf);
  let i = parse_input(&si);
 
  println!("P1 ans: {:?}", solve_p1(&f, &i));
  println!("P1 ans: {:?}", solve_p2(&f, &i));
}
