use std::fs::read_to_string;
use std::collections::HashMap;

#[derive(Clone)]
enum Expr<'a> {
  Val(&'a str, isize),
  Expr(&'a str, &'a str, &'a str, &'a str),
  Assert(&'a str, &'a str),
}

fn parse<'a>(i: &'a str) -> Vec<Expr<'a>> {
  i.lines().map(|l|{
    let va: Vec<_> = l.split(": ").collect();
    assert!(va.len() == 2);
    let ve: Vec<_> = va[1].split(' ').collect();
    match ve.as_slice() {
      [v]         => Expr::Val(va[0], v.parse().expect("number")),
      [l, op, r] => Expr::Expr(va[0], l, op, r),
      _           => panic!("Unhandled {:?}", ve),
    }

  }).collect()
}

fn solve_p1(i: &Vec<Expr>) -> isize {
  let mut known: HashMap<&str, isize> = HashMap::new();

  loop {
    if let Some(v) = known.get("root") { return *v }

    for e in i {
      match e {
        Expr::Val(t, v) => { known.insert(t, *v); },
        Expr::Expr(t, l, op, r) => if let (Some(lv), Some(rv)) = (known.get(l), known.get(r)) {
          let tv = match op {
            &"+" => *lv + *rv,
            &"-" => *lv - *rv,
            &"*" => *lv * *rv,
            &"/" => *lv / *rv,
            _   => panic!("Unknown {} operator", op)
          };
          known.insert(t, tv);
        }
        Expr::Assert(_, _) => panic!("Assert is not expected in P1")
      }
    }
  }
}

fn solve_p2(oi: &Vec<Expr>) -> isize {
  let mut i: Vec<Expr> = oi.iter().cloned().filter(|e| match e {
    // drop unknown variable
    Expr::Val("humn", _) => false, _ => true
  }).collect();

  for v in &mut i {
    // rewrite operators into an assert
    if let Expr::Expr("root", l, _, r) = v { *v = Expr::Assert(l, r) }
  }

  let mut known: HashMap<&str, isize> = HashMap::new();

  loop {
    if let Some(v) = known.get("humn") { return *v }

    for e in &i {
      match e {
        Expr::Val(t, v) => { known.insert(t, *v); },
        Expr::Expr(t, l, op, r) => {
          // direct calculation
          if let (Some(lv), Some(rv)) = (known.get(l), known.get(r)) {
            let tv = match op {
              &"+" => *lv + *rv,
              &"-" => *lv - *rv,
              &"*" => *lv * *rv,
              &"/" => *lv / *rv,
              _   => panic!("Unknown {} operator", op)
            };
            known.insert(t, tv);
          }
          // reverse calculation frov result and left operand:
          // tv = lv <op> rv
          if let (Some(tv), Some(lv)) = (known.get(t), known.get(l)) {
            let rv = match op {
              &"+" => *tv - *lv,
              &"-" => *lv - *tv,
              &"*" => *tv / *lv,
              &"/" => *lv / *tv, // TODO: imprecise if divided with truncation
              _   => panic!("Unknown {} operator", op)
            };
            known.insert(r, rv);
          }
          // reverse calculation frov result and right operand:
          // tv = lv <op> rv
          if let (Some(tv), Some(rv)) = (known.get(t), known.get(r)) {
            let lv = match op {
              &"+" => *tv - *rv,
              &"-" => *rv + *tv,
              &"*" => *tv / *rv,
              &"/" => *rv * *tv, // TODO: imprecise if divided with truncation
              _   => panic!("Unknown {} operator", op)
            };
            known.insert(l, lv);
          }
        }
        Expr::Assert(l, r) => {
          if let Some(lv) = known.get(l) { known.insert(r, *lv); }
          if let Some(rv) = known.get(r) { known.insert(l, *rv); }
        }
      }
    }
  }
}

fn main() {
  for input_file in ["example", "input"] {
    let input = read_to_string(input_file).expect("input");

    let prog = parse(&input);

    println!("{}: P1 ans: {:?}", input_file, solve_p1(&prog));
    println!("{}: P2 ans: {:?}", input_file, solve_p2(&prog));
  }
}
