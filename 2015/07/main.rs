use std::fs::read_to_string;
use std::collections::HashMap;

type Signal = u16;
type Wire = String;

#[derive(Clone, Debug)]
enum Gate {
  Assign(Signal, Wire), // <digit> -> <id>
  Copy(Wire, Wire), // <wire> -> <id>
  And(Wire, Wire, Wire), // <id> AND <id> -> id
  AndI(Signal, Wire, Wire), // <digit> AND <id> -> id
  Or(Wire, Wire, Wire), // <id> OR <id> -> id
  LShift(Wire, usize, Wire), // <id> LSHIFT <digit> -> id
  RShift(Wire, usize, Wire), // <id> RSHIFT <digit> -> id
  Not(Wire, Wire), // Not <id> -> id
}

type Kit = Vec<Gate>;

fn w(s: &str) -> Wire {
  assert!(!s.parse::<isize>().is_ok());
  s.to_string()
}

fn parse(s: &str) -> Kit {
  s.lines().map(|l|{
    let v: Vec<_> = l.split(' ').collect();
    match v.as_slice() {
      [d, "->", t] if d.parse::<u16>().is_ok() => Gate::Assign(d.parse().expect("digit"), t.to_string()),
      [s, "->", t]                => Gate::Copy(w(s), w(t)),
      [d, "AND",    s,  "->", t] if d.parse::<u16>().is_ok() => Gate::AndI(d.parse().expect("digit"), w(s), w(t)),
      [s1, "AND",   s2, "->", t] => Gate::And(w(s1), w(s2), w(t)),
      [s1, "OR",    s2, "->", t] => Gate::Or(w(s1), w(s2), w(t)),
      [s, "LSHIFT", d,  "->", t] => Gate::LShift(w(s), d.parse().expect("digit"), w(t)),
      [s, "RSHIFT", d,  "->", t] => Gate::RShift(w(s), d.parse().expect("digit"), w(t)),
      [    "NOT",   s,  "->", t] => Gate::Not(w(s), w(t)),
      _                           => panic!("Unsupported op: {:?}", v)
    }
  }).collect()
}

fn solve_p1(k: &Kit) -> Option<Signal> {
  let mut signals: HashMap<&String, Signal> = HashMap::new();

  loop {
    let mut progress = false;

    for gate in k {
      let s: Option<(&String, Signal)> =
        match gate {
          Gate::Assign(v, t) => {
            Some((t, *v))
          }
          Gate::Copy(s, t) => {
            match signals.get(s) {
              Some(sv) => Some((t, *sv)),
              _        => None,
            }
          }
          Gate::And(s1, s2, t) => {
            match (signals.get(s1), signals.get(s2)) {
              (Some(v1), Some(v2)) => Some((t, *v1 & *v2)),
              _                    => None,
            }
          }
          Gate::AndI(d, s, t) => {
            match signals.get(s) {
              Some(v) => Some((t, *v & d)),
              _ => None,
            }
          }
          Gate::Or(s1, s2, t) => {
            match (signals.get(s1), signals.get(s2)) {
              (Some(v1), Some(v2)) => Some((t, *v1 | *v2)),
              _                    => None,
            }
          }
          Gate::LShift(s, v, t) => {
            match signals.get(s) {
              Some(sv) => Some((t, *sv << *v)),
              _        => None,
            }
          }
          Gate::RShift(s, v, t) => {
            match signals.get(s) {
              Some(sv) => Some((t, *sv >> *v)),
              _        => None,
            }
          }
          Gate::Not(s, t) => {
            match signals.get(s) {
              Some(sv) => Some((t, !*sv)),
              _        => None,
            }
          }
        };

      if let Some((t, v)) = s {
        if let Some(ov) = signals.get_mut(t) {
          if *ov != v {
            assert!(true); /* rewrite */
            *ov = v;
            progress = true;
          }
       } else {
         signals.insert(t, v);
         progress = true;
       }
      }
    }

    if !progress { break }
  }

  signals.get(&"a".to_string()).copied()
}

fn solve_p2(k: &Kit, bsig: &Option<Signal>) -> Option<Signal> {
  let b = bsig.expect("b signal passed");

  let new_k = k.iter().map(|e| match e {
    Gate::Assign(_, t) if t == "b" => Gate::Assign(b, t.clone()),
    _ => e.clone(),
  }).collect();

  solve_p1(&new_k)
}

fn main() {
  for input_file in ["input"] {
    let i = read_to_string(input_file).expect("all ok");

    let kit = parse(&i);

    let ans_p1 = solve_p1(&kit);
    println!("{}: P1 ans: {:?}", input_file, ans_p1);

    let ans_p2 = solve_p2(&kit, &ans_p1);
    println!("{}: P2 ans: {:?}", input_file, ans_p2);
  }
}
