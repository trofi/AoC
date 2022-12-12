use std::fs::read_to_string;

type Pos = (usize, usize);

struct Range {
  lu: Pos,
  rd: Pos,
}

enum Op {
  On,
  Off,
  Toggle,
}

struct Cmd {
  op: Op,
  range: Range,
}

fn s2pos(s: &str) -> Pos {
  let v: Vec<usize> =
    s.split(',')
     .map(|e| e.parse().expect("number"))
     .collect();
  assert!(v.len() == 2);
 
  (v[0], v[1])
}

fn parse(s: &str) -> Vec<Cmd> {
  s.lines().map(|l| {
    let v: Vec<_> = l.split(' ').collect();
    match v.as_slice() {
      ["turn", "on", slu, "through", srd] => Cmd {
        op: Op::On,
        range: Range { lu: s2pos(slu), rd: s2pos(srd) },
      },
      ["turn", "off", slu, "through", srd] => Cmd {
        op: Op::Off,
        range: Range { lu: s2pos(slu), rd: s2pos(srd) },
      },
      ["toggle", slu, "through", srd] => Cmd {
        op: Op::Toggle,
        range: Range { lu: s2pos(slu), rd: s2pos(srd) },
      },
      _ => panic!("Unknown {:?} cmd", v),
    }
  }).collect()
}

fn solve_p1(i: &Vec<Cmd>) -> usize {
  let (cols, rows) = (1000, 1000);
  let mut field: Vec<Vec<bool>> = Vec::new();

  for r in 0..rows {
    let mut v = Vec::new();
    v.resize(cols, false);
    field.push(v);
  }

  for Cmd{op, range} in i {
    for c in range.lu.0..=range.rd.0 {
      for r in range.lu.1..=range.rd.1 {
        match op {
          Op::On => field[c][r] = true,
          Op::Off => field[c][r] = false,
          Op::Toggle => field[c][r] = !field[c][r],
        }
      }
    }
  }

  field.iter().map(|r|
    r.iter().filter(|e| **e).count()
  ).sum()
}

fn solve_p2(i: &Vec<Cmd>) -> usize {
  let (cols, rows) = (1000, 1000);
  let mut field: Vec<Vec<usize>> = Vec::new();

  for r in 0..rows {
    let mut v = Vec::new();
    v.resize(cols, 0);
    field.push(v);
  }

  for Cmd{op, range} in i {
    for c in range.lu.0..=range.rd.0 {
      for r in range.lu.1..=range.rd.1 {
        match op {
          Op::On => field[c][r] += 1,
          Op::Off => if  field[c][r] > 0 { field[c][r] -= 1 },
          Op::Toggle => field[c][r] += 2,
        }
      }
    }
  }

  field.iter().map(|r|
    r.iter().sum::<usize>()
  ).sum()
}

fn main() {
  for input_file in ["example", "input"] {
    let i = read_to_string(input_file).expect("all ok");

    let cmds = parse(&i);

    let ans_p1 = solve_p1(&cmds);
    println!("{}: P1 ans: {:?}", input_file, ans_p1);
    let ans_p2 = solve_p2(&cmds);
    println!("{}: P2 ans: {:?}", input_file, ans_p2);
  }
}
