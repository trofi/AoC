use std::fs::read_to_string;
use std::error::Error;
use std::collections::VecDeque;

type E = Box<dyn Error>;

struct Monkey {
  items: VecDeque<usize>,
  op: Box<dyn Fn(usize) -> usize>,
  test_div_op: usize,
  test_yes: usize,
  test_no: usize,
}

fn op_to_fn(op: &str) -> fn(usize, usize) -> usize {
  match op {
    "*" => |l, r| { l * r },
    "+" => |l, r| { l + r },
    _   => panic!("unhandled '{:?}' arith op", op),
  }
}

fn parse(i: &str) -> Vec<Monkey> {
  i.split("\n\n").map(|cmd| {
    /* expect all of them to be present */
    let mut items       = None;
    let mut op: Option<Box<dyn Fn(usize) -> usize>>  = None;
    let mut test_div_op = None;
    let mut test_yes    = None;
    let mut test_no     = None;

    for l in cmd.lines() {
      let v: Vec<_> = l.split(": ").collect();

      match v.as_slice() {
        _ if l.starts_with("Monkey ") => (),
        &["  Starting items", is] => {
          items = Some(is.split(", ").map(|e| e.parse().expect("number")).collect());
        },
        &["  Operation", sop] => {
          let vop: Vec<_> = sop.split(' ').collect();
          /* TODO: do we need full expression parser or can get away with trivial arithmentcs? */
          match vop.as_slice() {
            &["new", "=", "old", o, "old"] => {
              let f = op_to_fn(o);
              op = Some(Box::new(move |old: usize| { f(old, old) }));
            },
            &["new", "=", "old", o, ns] => {
              let n = ns.parse().expect("number");
              let f = op_to_fn(o);
              op = Some(Box::new(move |old: usize| { f(old, n) }));
            },
            _ => panic!("unhandled '{:?}' operation", vop),
          }
        },
        &["  Test", sop] => {
          let vop: Vec<_> = sop.split(' ').collect();
          /* TODO: do we need full expression parser or can get away with trivial arithmentcs? */
          match vop.as_slice() {
            &["divisible", "by", ns] => {
              test_div_op = Some(ns.parse().expect("number"));
            },
            _ => panic!("unhandled '{:?}' test operation", vop),
          }
        },
        &["    If true", sop] => {
          let vop: Vec<_> = sop.split(' ').collect();
          match vop.as_slice() {
            &["throw", "to", "monkey", sn] => {
              test_yes = Some(sn.parse().expect("number"));
            },
            _ => panic!("unhandled '{:?}' command", vop),
          }
        }
        &["    If false", sop] => {
          let vop: Vec<_> = sop.split(' ').collect();
          match vop.as_slice() {
            &["throw", "to", "monkey", sn] => {
              test_no = Some(sn.parse().expect("number"));
            },
            _ => panic!("unhandled '{:?}' command", vop),
          }
        }
        _ => panic!("unhandled '{:?}' command", v),
      }
    }

    Monkey {
      items:       items.expect("present 'Starting items'"),
      op:          op.expect("present 'Operation'"),
      test_div_op: test_div_op.expect("present 'Test'"),
      test_yes:    test_yes.expect("present 'If true'"),
      test_no:     test_no.expect("present 'If false'"),
    }
  }).collect()
}

fn solve(monkeys: Vec<Monkey>, rounds: usize, worry_divider: usize) -> usize {
  let mut state: Vec<Monkey> = monkeys;

  let mut inspections: Vec<usize> = Vec::new();
  inspections.resize(state.len(), 0);

  let max_mod: usize = state.iter().map(|m| m.test_div_op).product();

  for _ in 0..rounds {
    for mix in 0..state.len() {
      inspections[mix] += state[mix].items.len();

      while !state[mix].items.is_empty() {
        let mut v = state[mix].items.pop_front().expect("at least one item");
        v = (state[mix].op)(v);

        v = v / worry_divider;

        if worry_divider == 1 {
          // Modular arithmetics can only be used on '*' / '+' field (without divisions).
          v = v % max_mod;
        }

        let b = v % state[mix].test_div_op == 0;
        let target = if b { state[mix].test_yes } else { state[mix].test_no };
        state[target].items.push_back(v);
      }
    }

  }

  inspections.sort();
  inspections.iter().rev().take(2).product()
}

fn main() -> Result<(), E> {
  for ifile in ["example" , "input"] {
    let input = read_to_string(ifile)?;

    let monkeys = parse(&input);
    let ans_p1 = solve(monkeys, 20, 3);
    println!("{}: P1 ans: {:?}", ifile, ans_p1);

    let monkeys = parse(&input);
    let ans_p2 = solve(monkeys, 10000, 1);
    println!("{}: P2 ans: {:?}", ifile, ans_p2);
  }
  Ok(())
}
