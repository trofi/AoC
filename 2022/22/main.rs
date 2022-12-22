use std::fs::read_to_string;
use std::collections::HashMap;

type Pos = (isize, isize);

#[derive(Debug)]
enum Cmd {
  R,
  L,
  Move(isize),
}

struct Input {
  map: HashMap<Pos, char>,
  trail: Vec<Cmd>,
}

fn parse(i: &str) -> Input {
  let mp: Vec<_> = i.split("\n\n").collect();
  assert!(mp.len() == 2);

  let mut map: HashMap<Pos, char> = HashMap::new();

  for (r, l) in mp[0].lines().enumerate() {
    for (c, v) in l.chars().enumerate() {
      if v != ' ' {
        map.insert((r as isize, c as isize), v);
      }
    }
  }

  let mut trail: Vec<Cmd> = Vec::new();
  let mut l = 0;
  for c in mp[1].chars() {
    match c {
      'L' => { trail.push(Cmd::Move(l)); trail.push(Cmd::L); l = 0; }
      'R' => { trail.push(Cmd::Move(l)); trail.push(Cmd::R); l = 0; }
      _   => l = 10 * l + c.to_digit(10).expect("a digit") as isize
    }
  }
  trail.push(Cmd::Move(l));

  Input{
    map: map,
    trail: trail,
  }
}

struct State {
  pos: Pos,
  dir: usize,
}

fn solve_p1(Input{map,trail}: &Input) -> isize {
  let mut s = State {
    pos: *map.keys().min().expect("at least one position"),
    dir: 0,
  };
  assert!(map.get(&s.pos) == Some(&'.'));

  let rl = *map.keys().map(|(r, _)| r).max().unwrap() + 1;
  let cl = *map.keys().map(|(_, c)| c).max().unwrap() + 1;

  const DIRS: &[char] = &['>', 'v', '<', '^'];

  for cmd in trail {
    match cmd {
      Cmd::L       => s.dir = (s.dir + DIRS.len() - 1) % DIRS.len(),
      Cmd::R       => s.dir = (s.dir              + 1) % DIRS.len(),
      Cmd::Move(l) => {
        for _ in 0..*l {
          let mut np: Pos = s.pos;
          loop {
            np = match DIRS[s.dir] {
              '>' => (np.0,                 (np.1 + 1) % cl),
              'v' => ((np.0 + 1) % rl,      np.1),
              '<' => (np.0,                 (np.1 + cl - 1) % cl),
              '^' => ((np.0 + rl - 1) % rl, np.1),
              c   => panic!("Unhandled '{}' direction", c)
            };
            if map.get(&np).is_some() { break }
          }
          if map.get(&np) == Some(&'.') {
            s.pos = np;
          }
        }
      }
    }
  }

  1000 * (s.pos.0 + 1)
  + 4 * (s.pos.1 + 1)
  + (s.dir as isize)
}

fn main() {
  for input_file in ["example", "input"] {
    let input = read_to_string(input_file).expect("input");

    let i = parse(&input);

    println!("{}: P1 ans: {:?}", input_file, solve_p1(&i));
  }
}
