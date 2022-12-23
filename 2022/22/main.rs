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

#[derive(Debug, Copy, Clone)]
enum Dir {
  Right,
  Down,
  Left,
  Up,
}

impl Dir {
  fn r(self: Self) -> Self {
    match self {
      Dir::Right => Dir::Down,
      Dir::Down  => Dir::Left,
      Dir::Left  => Dir::Up,
      Dir::Up    => Dir::Right,
    }
  }

  fn l(self: Self) -> Self {
    match self {
      Dir::Right => Dir::Up,
      Dir::Down  => Dir::Right,
      Dir::Left  => Dir::Down,
      Dir::Up    => Dir::Left,
    }
  }
}

struct State {
  pos: Pos,
  dir: Dir,
}

fn solve_p1(Input{map,trail}: &Input) -> isize {
  let mut s = State {
    pos: *map.keys().min().expect("at least one position"),
    dir: Dir::Right,
  };
  assert!(map.get(&s.pos) == Some(&'.'));

  let rl = *map.keys().map(|(r, _)| r).max().unwrap() + 1;
  let cl = *map.keys().map(|(_, c)| c).max().unwrap() + 1;

  for cmd in trail {
    match cmd {
      Cmd::L       => s.dir = s.dir.l(),
      Cmd::R       => s.dir = s.dir.r(),
      Cmd::Move(l) => {
        for _ in 0..*l {
          let mut np: Pos = s.pos;
          loop {
            np = match s.dir {
              Dir::Right => (np.0,                 (np.1 + 1) % cl),
              Dir::Down  => ((np.0 + 1) % rl,      np.1),
              Dir::Left  => (np.0,                 (np.1 + cl - 1) % cl),
              Dir::Up    => ((np.0 + rl - 1) % rl, np.1),
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


#[derive(Copy, Clone)]
enum LayoutHack {
  Example,
  Input,
}
/*
  TODO: should auto-generate this wraparound. So far manually wrap using the following scheme:


  Layout 2 (input):

   +012+
   0 EF|
   1 D |
   2BC |
   3A  |
   +---+
*/

fn divmod(n: isize, m: isize) -> (isize, isize) {
  let q = n / m;
  let r = n % m;
  if r < 0 { (q - 1, r + m) } else { (q, r) }
}

fn wrap_pos((r, c): &Pos, dir: Dir, l: isize, layout_hack: LayoutHack) -> (Pos, Dir) {
  let (qr, dr) = divmod(*r, l);
  let (qc, dc) = divmod(*c, l);

  assert!(dr >= 0);
  assert!(dc >= 0);

  //println!("q={:?} d={:?}", (qr, qc), (dr, dc));

  match layout_hack {
    /*
       +0123+
       0  D |
       1ABC |
       2  EF|
       +----+
    */
    LayoutHack::Example => match (qr, qc, dir) {
      /* 'C >' to 'F v' */ (1, 3, Dir::Right) => ((2 * l,          4 * l - 1 - dr), Dir::Down ),
      /* 'F v' to 'A >' */ (3, 3, Dir::Down ) => ((2 * l - 1 - dc, 0             ), Dir::Right),
      /* 'A v' to 'E ^' */ (2, 0, Dir::Down ) => ((3 * l - 1,      3 * l - 1 - dc), Dir::Up   ),
      /* 'E v' to 'A ^' */ (3, 2, Dir::Down ) => ((2 * l - 1,      l - 1 - dc    ), Dir::Up   ),
      /* 'B ^' to 'D >' */ (0, 1, Dir::Up   ) => ((dc,             2 * l         ), Dir::Right),
      v => panic!("Unhandled example (qr,qc)={:?} layout (dr,dc)={:?}", v, (dr, dc)),
    }
    /*
       +012+
       0 EF|
       1 D |
       2BC |
       3A  |
       +---+
    */
    LayoutHack::Input => match ((qr, qc), dir) {
      /* 'E <' to 'B >' */ ((0,  0), Dir::Left ) => ((3 * l - 1 - dr, 0         ), Dir::Right),
      /* 'B <' to 'E >' */ ((2, -1), Dir::Left ) => ((l - 1 - dr,     l         ), Dir::Right),

      /* 'A v' to 'F v' */ ((4,  0), Dir::Down ) => ((0,              2 * l + dc), Dir::Down ),
      /* 'F ^' to 'A ^' */ ((-1, 2), Dir::Up   ) => ((4 * l - 1,      dc        ), Dir::Up   ),

      /* 'A >' to 'C ^' */ ((3,  1), Dir::Right) => ((3 * l - 1,      l + dr    ), Dir::Up   ),
      /* 'C v' to 'A <' */ ((3,  1), Dir::Down ) => ((3 * l + dc,     l - 1     ), Dir::Left ),

      /* 'C >' to 'F <' */ ((2,  2), Dir::Right) => ((l - 1 - dr,     3 * l - 1 ), Dir::Left ),
      /* 'F >' to 'C <' */ ((0,  3), Dir::Right) => ((3 * l - 1 - dr, 2 * l - 1 ), Dir::Left ),

      /* 'B ^' to 'D >' */ ((1,  0), Dir::Up   ) => ((l + dc,         l         ), Dir::Right),
      /* 'D <' to 'B v' */ ((1,  0), Dir::Left ) => ((2 * l,          dr        ), Dir::Down ),

      /* 'E ^' to 'A >' */ ((-1, 1), Dir::Up   ) => ((3 * l + dc,     0         ), Dir::Right),
      /* 'A <' to 'E v' */ ((3, -1), Dir::Left ) => ((0,              l + dr    ), Dir::Down),

      /* 'F v' to 'D <' */ ((1,  2), Dir::Down ) => ((l + dc,         2 * l - 1 ), Dir::Left ),
      /* 'D >' to 'F ^' */ ((1,  2), Dir::Right) => ((l - 1,          2 * l + dr), Dir::Up   ),

      v => panic!("Unhandled example (qr,qc)={:?} layout (dr,dc)={:?}", v, (dr, dc)),
    }
  }
}

fn solve_p2(Input{map,trail}: &Input, layout_hack: LayoutHack) -> isize {
  let mut s = State {
    pos: *map.keys().min().expect("at least one position"),
    dir: Dir::Right,
  };
  assert!(map.get(&s.pos) == Some(&'.'));

  assert!(map.len() % 6 == 0);
  let cube_side = ((map.len() / 6) as f64).sqrt() as isize;
  println!("Cube side: {cube_side}");

  for cmd in trail {
    match cmd {
      Cmd::L       => s.dir = s.dir.l(),
      Cmd::R       => s.dir = s.dir.r(),
      Cmd::Move(l) => {
        for _ in 0..*l {
          let mut np: Pos = s.pos;
          let mut np_dir: Dir = s.dir;
          loop {
            np = match s.dir {
              Dir::Right => (np.0, np.1 + 1),
              Dir::Down => (np.0 + 1, np.1),
              Dir::Left => (np.0, np.1 - 1),
              Dir::Up => (np.0 - 1, np.1),
            };
            if map.get(&np).is_some() { break }
            (np, np_dir) = wrap_pos(&np, np_dir, cube_side, layout_hack);
            if map.get(&np).is_some() { break }
          }
          if map.get(&np) == Some(&'.') {
            //println!("Move pos:{:?}->{:?} dir:{:?}->{:?}", s.pos, np, s.dir, np_dir);
            s.pos = np;
            s.dir = np_dir;
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
  for (layout_hack, input_file) in [(LayoutHack::Example, "example"), (LayoutHack::Input, "input")] {
    let input = read_to_string(input_file).expect("input");

    let i = parse(&input);

    println!("{}: P1 ans: {:?}", input_file, solve_p1(&i));
    println!("{}: P2 ans: {:?}", input_file, solve_p2(&i, layout_hack));
  }
}
