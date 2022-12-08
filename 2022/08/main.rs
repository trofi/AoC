use std::fs::read_to_string;
use std::error::Error;

type E = Box<dyn Error>;

#[derive(Debug)]
struct State {
  height: u32,
  is_visible: bool, /* Unknown or Seen. */
}
type Grid = Vec<Vec<State>>;

fn h(f: &Grid) -> usize { f.len() }
fn w(f: &Grid) -> usize { f[0].len() }

fn parse(i: &str) -> Grid {
  i.lines().map(|l|
    l.chars().map(|c| {
      let h = c.to_digit(10).expect("digit");
      State{
        height: h,
        is_visible: false,
      }
    }).collect()
  ).collect()
}

fn solve_p1(f: &mut Grid) -> usize {
  /* cast up */
  for c in 0..w(f) {
    let mut max_hei = None;
    for r in 0..h(f) {
      let hei = Some(f[r][c].height);
      if hei > max_hei {
        f[r][c].is_visible = true;
        max_hei = hei;
      }
    }
  }
  /* down */
  for c in 0..w(f) {
    let mut max_hei = None;
    for r in (0..h(f)).rev() {
      let hei = Some(f[r][c].height);
      if hei > max_hei {
        f[r][c].is_visible = true;
        max_hei = hei;
      }
    }
  }
  /* left */
  for r in 0..h(f) {
    let mut max_hei = None;
    for c in 0..w(f) {
      let hei = Some(f[r][c].height);
      if hei > max_hei {
        f[r][c].is_visible = true;
        max_hei = hei;
      }
    }
  }
  /* right */
  for r in 0..h(f) {
    let mut max_hei = None;
    for c in (0..w(f)).rev() {
      let hei = Some(f[r][c].height);
      if hei > max_hei {
        f[r][c].is_visible = true;
        max_hei = hei;
      }
    }
  }

  let mut visible = 0;

  for c in 0..w(f) {
    for r in 0..h(f) {
      if f[r][c].is_visible {
        visible += 1;
      }
    }
  }

  visible
}

fn solve_p2(f: &Grid) -> usize {
  let mut candidates = Vec::new();

  for r in 0..h(f) {
    for c in 0..w(f) {
      if f[r][c].is_visible {
        candidates.push((r, c));
      }
    }
  }

  candidates.iter().map(|(r,c)| {
    let hei = f[*r][*c].height;
    /* assume we bump into a high tree, or a bound */
    let up = (0..*r)
        .rev()
        .enumerate()
        .find(|(_, i)| f[*i][*c].height >= hei)
        .map_or(*r, |(n, _)| n + 1);
    let down = (*r+1..h(f))
        .enumerate()
        .find(|(_, i)| f[*i][*c].height >= hei)
        .map_or(h(f) - *r - 1, |(n, _)| n + 1);
    let left = (0..*c)
        .rev()
        .enumerate()
        .find(|(_, i)| f[*r][*i].height >= hei)
        .map_or(*c, |(n, _)| n + 1);
    let right = (*c+1..w(f))
        .enumerate()
        .find(|(_, i)| f[*r][*i].height >= hei)
        .map_or(w(f) - *c - 1, |(n, _)| n + 1);

    up * down * left * right
  }).max().expect("at least one element")
}

fn main() -> Result<(), E> {
  for ifile in ["example" , "input"] {
    let input = read_to_string(ifile)?;

    let mut f = parse(&input);

    let ans = solve_p1(&mut f);
    println!("{}: P1 ans: {:?}", ifile, ans);
    let ans_p2 = solve_p2(&f);
    println!("{}: P2 ans: {:?}", ifile, ans_p2);
  }
  Ok(())
}
