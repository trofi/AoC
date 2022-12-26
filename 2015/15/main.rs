use std::cmp::max;

fn solve_example_p1() -> isize {
  let mut r = 0;

  for c1 in 0..=100 {
    let c2 = 100-c1;
    // capacity -1, durability -2, flavor  6, texture  3, calories 8
    // capacity  2, durability  3, flavor -2, texture -1, calories 3
    let cap = -1 * c1 + 2 * c2;
    let dur = -2 * c1 + 3 * c2;
    let fla =  6 * c1 - 2 * c2;
    let tex =  3 * c1 - 1 * c2;

    let score = max(0, cap) * max(0, dur) * max(0, fla) * max(0, tex);

    r = max(r, score);
  }

  r
}

fn solve_example_p2() -> isize {
  let mut r = 0;

  for c1 in 0..=100 {
    let c2 = 100-c1;
    // capacity -1, durability -2, flavor  6, texture  3, calories 8
    // capacity  2, durability  3, flavor -2, texture -1, calories 3
    let cap = -1 * c1 + 2 * c2;
    let dur = -2 * c1 + 3 * c2;
    let fla =  6 * c1 - 2 * c2;
    let tex =  3 * c1 - 1 * c2;

    let score = max(0, cap) * max(0, dur) * max(0, fla) * max(0, tex);

    let cal =  8 * c1 + 3 * c2;
    if cal == 500 {
      r = max(r, score);
    }
  }

  r
}

fn solve_p1() -> isize {
  let mut r = 0;

  for c1 in 0..=100 {
    for c2 in 0..=100-c1 {
      for c3 in 0..=100-c1-c2 {
        let c4 = 100-c1-c2-c3;
        // capacity 2, durability  0, flavor -2, texture  0, calories 3
        // capacity 0, durability  5, flavor -3, texture  0, calories 3
        // capacity 0, durability  0, flavor  5, texture -1, calories 8
        // capacity 0, durability -1, flavor  0, texture  5, calories 8
        let cap =  2 * c1 + 0 * c2 + 0 * c3 + 0 * c4;
        let dur =  0 * c1 + 5 * c2 + 0 * c3 - 1 * c4;
        let fla = -2 * c1 - 3 * c2 + 5 * c3 + 0 * c4;
        let tex =  0 * c1 + 0 * c2 - 1 * c3 + 5 * c4;

        let score = max(0, cap) * max(0, dur) * max(0, fla) * max(0, tex);

        r = max(r, score);
      }
    }
  }

  r
}

fn solve_p2() -> isize {
  let mut r = 0;

  for c1 in 0..=100 {
    for c2 in 0..=100-c1 {
      for c3 in 0..=100-c1-c2 {
        let c4 = 100-c1-c2-c3;
        // capacity 2, durability  0, flavor -2, texture  0, calories 3
        // capacity 0, durability  5, flavor -3, texture  0, calories 3
        // capacity 0, durability  0, flavor  5, texture -1, calories 8
        // capacity 0, durability -1, flavor  0, texture  5, calories 8
        let cap =  2 * c1 + 0 * c2 + 0 * c3 + 0 * c4;
        let dur =  0 * c1 + 5 * c2 + 0 * c3 - 1 * c4;
        let fla = -2 * c1 - 3 * c2 + 5 * c3 + 0 * c4;
        let tex =  0 * c1 + 0 * c2 - 1 * c3 + 5 * c4;

        let score = max(0, cap) * max(0, dur) * max(0, fla) * max(0, tex);

        let cal =  3 * c1 + 3 * c2 + 8 * c3 + 8 * c4;
        if cal == 500 {
          r = max(r, score);
        }
      }
    }
  }

  r
}

fn main() {
  println!("ex P1 ans: {:?}", solve_example_p1());
  println!("ex P2 ans: {:?}", solve_example_p2());
  println!("in P1 ans: {:?}", solve_p1());
  println!("in P2 ans: {:?}", solve_p2());
}
