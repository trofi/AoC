fn solve_p1(n: usize) -> usize {
  let max_house = n / 10 + 1;

  let mut v: Vec<usize> = Vec::new();
  v.resize(max_house, 0);

  let mut r = usize::MAX;

  for elf in 1..max_house {
    for ix in 1.. {
      let h = elf * ix;
      let p = elf * 10;
      if h >= max_house { break }

      v[h] = std::cmp::min(v[h] + p, n);

      if v[h] == n {
        if h < r {
          println!("{} -> {}", r, h);
        }
        r = std::cmp::min(r, h);
      }
    }
  }

  r
}

fn solve_p2(n: usize) -> usize {
  let max_house = n / 10 + 1;

  let mut v: Vec<usize> = Vec::new();
  v.resize(max_house, 0);

  let mut r = usize::MAX;

  for elf in 1..max_house {
    for ix in 1..=50 {
      let h = elf * ix;
      let p = elf * 11;
      if h >= max_house { break }

      v[h] = std::cmp::min(v[h] + p, n);

      if v[h] == n {
        if h < r {
          println!("{} -> {}", r, h);
        }
        r = std::cmp::min(r, h);
      }
    }
  }

  r
}

fn main() {
  println!("P1 ans: {:?}", solve_p1(34000000));
  println!("P1 ans: {:?}", solve_p2(34000000));
}
