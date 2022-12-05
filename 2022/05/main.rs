use std::fs::read_to_string;
use std::iter::FromIterator;
use std::str::FromStr;
use std::error::Error;

type E = Box<dyn Error>;
fn mk_e(err: String) -> E {
  Box::<dyn Error>::from(err)
}

struct Op {
  count: usize,
  from:  usize,
  to:    usize,
}

struct Input {
  field: Vec<Vec<char>>,
  ops: Vec<Op>,
}

impl FromStr for Input {
  type Err = E;
  fn from_str(s: &str) -> Result<Self, Self::Err> {
    let (field_ss, ops_s): (_, _) =
      match s.split("\n\n").collect::<Vec<_>>().as_slice() {
        &[f_s, o_s] => Ok((f_s.lines().map(|l| l.chars().collect::<Vec<char>>()).collect::<Vec<_>>(), o_s)),
        v           => Err(mk_e(format!("Found {} separators, expected 1", v.len())))
      }?;

    let w = (field_ss[0].len() + 1) / 4;
    let h = field_ss.len() - 1; /* skip numbers */

    let f = (0..w).map(|i|
      (0..h).rev().map(|j|
        field_ss[j][4*i + 1]
      ).filter(|e| *e != ' ').collect()
    ).collect();

    let os = ops_s.lines().map(|l|
      match l.split(' ').collect::<Vec<_>>().as_slice() {
        &["move", c_s, "from", f_s, "to", t_s] =>
          Ok(Op{
               count: c_s.parse()?,
               from:  f_s.parse()?,
               to:    t_s.parse()?,}),
        _ => Err(mk_e(format!("Could not parse op {}", l)))
      }
    ).collect::<Result<Vec<_>, _>>()?;

    Ok(Input{field: f, ops: os})
  }
}

fn solve(i: &Input, is_p2: bool) -> Result<String, E> {
  let mut f = i.field.clone();

  for op in &i.ops {
    let mut v = Vec::new();
    for _ in 0..op.count {
      let c = f[op.from - 1].pop().expect("at least one elem in field");
      v.push(c);
    }

    if !is_p2 {
      for c in v {
        f[op.to - 1].push(c);
      }
    } else {
      for c in v.iter().rev() {
        f[op.to - 1].push(*c);
      }
    }
  }

  let v: Vec<char> = f.iter().map(|r|
    *r.last().expect("at least one element")
  ).collect();

  Ok(String::from_iter(v))
}

fn main() -> Result<(), E> {
  for ifile in ["example" , "input"] {
    let input_string = read_to_string(ifile)?;
    let input = input_string.parse()?;

    let ans_p1 = solve(&input, false)?;
    println!("{}: P1 ans: {}", ifile, ans_p1);
    let ans_p1 = solve(&input, true)?;
    println!("{}: P2 ans: {}", ifile, ans_p1);
  }
  Ok(())
}
