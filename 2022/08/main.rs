use std::fs::read_to_string;
use std::error::Error;
use std::cmp;

type E = Box<dyn Error>;

#[derive(Debug)]
struct State {
  height: u32,
  is_visible: bool, /* Unknown or Seen. */
}
type Grid = Vec<Vec<State>>;

fn h(g: &Grid) -> usize { g.len() }
fn w(g: &Grid) -> usize { g[0].len() }

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

  for c in 0..w(f) {
    for r in 0..h(f) {
      if f[r][c].is_visible {
        candidates.push((c, r));
      }
    }
  }

  candidates.iter().map(|(c,r)| {
    let hei = f[*r][*c].height;
    /* assume we bump into a high tree, will clamp to bounds below */
    let up = (0..*r).rev().take_while(|i| f[*i][*c].height < hei).count() + 1;
    let down = (*r..h(f)).take_while(|i| f[*i][*c].height < hei).count() + 1;
    let left = (0..*c).rev().take_while(|i| f[*r][*i].height < hei).count() + 1;
    let right = (*c..w(f)).take_while(|i| f[*r][*i].height < hei).count() + 1;

    /* clamp to bounds */
      cmp::min(up, *r)
    * cmp::min(down, h(f) - *r - 1)
    * cmp::min(left, *c)
    * cmp::min(right, w(f) - *c - 1)
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
