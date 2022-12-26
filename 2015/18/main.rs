use std::collections::HashSet;

type Pos = (isize, isize);

#[derive(Clone)]
struct Field {
  dim: Pos,
  m: HashSet<Pos>
}

fn parse(s: &str) -> Field {
  let mut f = Field {
    dim: (0, 0),
    m: HashSet::new(),
  };

  for (y, l) in s.lines().enumerate() {
    f.dim.1 = std::cmp::max(f.dim.1, y as isize + 1);
    for (x, c) in l.chars().enumerate() {
      f.dim.0 = std::cmp::max(f.dim.0, x as isize + 1);

      if c == '#' {
        f.m.insert((x as isize, y as isize));
      }
    }
  }

  f
}

fn round(f: &Field) -> Field {
  let mut r = Field {
    dim: f.dim,
    m: HashSet::new(),
  };

  for x in 0..f.dim.0 {
    for y in 0..f.dim.1 {
      let c =
        [(x - 1, y - 1), (x, y - 1), (x + 1, y - 1),
         (x - 1, y    ),             (x + 1, y    ),
         (x - 1, y + 1), (x, y + 1), (x + 1, y + 1)]
          .iter()
          .filter(|(nx, ny)| f.m.contains(&(*nx, *ny)))
          .count();
       if match f.m.contains(&(x, y)) { true  => c == 2 || c == 3,false => c == 3 } {
         r.m.insert((x,y));
       }
    }
  }

  r
}

fn solve_p1(f: &Field, steps: usize) -> usize {
  let mut mf: Field = f.clone();

  for _ in 0..steps {
    mf = round(&mf);
  }

  mf.m.len()
}

fn stick_on(f: &mut Field) {

  for x in [0, f.dim.0 - 1] {
    for y in [0, f.dim.1 - 1] {
      f.m.insert((x, y));
    }
  }
}

fn solve_p2(f: &Field, steps: usize) -> usize {
  let mut mf: Field = f.clone();
  stick_on(&mut mf);

  for _ in 0..steps {
    mf = round(&mf);
    stick_on(&mut mf);
  }

  mf.m.len()
}

fn main() {
  for (steps, input_file) in [(4, "example"), (5, "example"), (100, "input")] {
    let i = std::fs::read_to_string(input_file).expect("all ok");

    let field = parse(&i);

    println!("{}: P1 ans: {:?}", input_file, solve_p1(&field, steps));
    println!("{}: P2 ans: {:?}", input_file, solve_p2(&field, steps));
  }
}
