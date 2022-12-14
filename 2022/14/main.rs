use std::fs::read_to_string;
use std::collections::HashMap;
use std::cmp::max;
use std::cmp::min;

enum Sq {
  Rock,
  Sand,
  SandSource,
}

type Pos = (isize, isize);
type Field = HashMap<Pos, Sq>;

fn field_range(f: &Field) -> (Pos, Pos) {
  if f.is_empty() { return ((0,0), (0,0)); }

  let min_x = *f.keys().map(|(x, _)| x).min().unwrap();
  let min_y = *f.keys().map(|(_, y)| y).min().unwrap();

  let max_x = *f.keys().map(|(x, _)| x).max().unwrap();
  let max_y = *f.keys().map(|(_, y)| y).max().unwrap();

  ((min_x, min_y), (max_x, max_y))
}

fn parse(i: &str) -> Vec<Vec<Pos>> {
  i.lines().map(|l|
    l.split(" -> ")
     .map(|e|{
       let v: Vec<_> = e.split(',').collect();
       match v.as_slice() {
         [x, y] => (x.parse().expect("number"), y.parse().expect("number")),
         _      => panic!("Unexpected {} input", e)
       }
     })
     .collect()
  ).collect()
}

fn solve(cmds: &Vec<Vec<Pos>>, has_floor: bool) -> usize {
  let mut f = Field::new();
  let sand_source = (500, 0);

  f.insert(sand_source, Sq::SandSource);

  // sprinkle rocks
  for cmd in cmds {
    let mut prev = None;
    for target@(tx, ty) in cmd {
      match prev {
        None => {
          f.insert(*target, Sq::Rock);
        },
        Some((px, py)) => {
          if px == *tx {
            for y in min(py, *ty)..=max(py,*ty) {
              f.insert((px, y), Sq::Rock);
            }
          } else if py == *ty {
            for x in min(px, *tx)..=max(px,*tx) {
              f.insert((x, py), Sq::Rock);
            }
          } else {
            panic!("Can't handle diagonals yet: {:?}->{:?}.", prev, target)
          }
        },
      }
      prev = Some(*target);
    }
  }

  // add floor
  if has_floor {
    let ((_, _), (_, max_y)) = field_range(&f);
    let floor_y = max_y + 2;

    for x in (sand_source.0 - floor_y - 1)..=(sand_source.0 + floor_y + 1) {
      f.insert((x, floor_y), Sq::Rock);
    }
  }

  let in_range = {
    let ((min_x, min_y), (max_x, max_y)) = field_range(&f);
    move |(x, y)| {
         x >= min_x && x <= max_x
      && y >= min_y && y <= max_y
    }
  };

  let can_free_fall  = |f: &Field, (x, y)| !f.contains_key(&(x,     y + 1));
  let can_roll_left  = |f: &Field, (x, y)| !f.contains_key(&(x - 1, y + 1));
  let can_roll_right = |f: &Field, (x, y)| !f.contains_key(&(x + 1, y + 1));

  for i in 1.. {
    // simulate fall
    let mut p = sand_source;
    while in_range(p) {
      match () {
       _ if can_free_fall(&f, p)  => p = (p.0, p.1 + 1),
       _ if can_roll_left(&f, p)  => p = (p.0 - 1, p.1 + 1),
       _ if can_roll_right(&f, p) => p = (p.0 + 1, p.1 + 1),
       _ /* stuck */              => { f.insert(p.clone(), Sq::Sand); break; }
      }
    }

    if p == sand_source { return i; }
    if !in_range(p) { return i - 1; }
  }
  0 /* unreach */
}

fn main() {
  for input_file in ["example" , "input"] {
    let input = read_to_string(input_file).expect("input");

    let cmds = parse(&input);

    let ans_p1 = solve(&cmds, false);
    println!("{}: P1 ans: {:?}", input_file, ans_p1);
    let ans_p2 = solve(&cmds, true);
    println!("{}: P2 ans: {:?}", input_file, ans_p2);
  }
}
