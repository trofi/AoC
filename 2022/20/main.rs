use std::fs::read_to_string;

fn parse(i: &str) -> Vec<isize> {
  i.lines().map(|e| e.parse().expect("digit")).collect()
}

fn mix(i: &[isize], rounds: usize) -> isize {
  let ni: Vec<(usize, isize)> = i.iter().cloned().enumerate().collect();
  let mut v: Vec<(usize, isize)> = ni.clone();

  for _ in 0..rounds {
    for (i, n) in ni.iter().cloned() {
      let spos =
          v.iter()
           .enumerate()
           .find(|(_, (ix, _))| *ix == i)
           .unwrap().0;
      v.remove(spos);

      let l = v.len() as isize;
      let dpos = (spos as isize + n % l + l) % l;
      v.insert(dpos as usize, (i, n));
    }
  }

  let zero_pos =
      v.iter()
       .enumerate()
       .find(|(_, (_, e))| *e == 0)
       .expect("zero").0;

  [1000, 2000, 3000].iter().map(|ix|{
    v[(zero_pos + ix) % v.len()].1
  }).sum()
}

fn solve_p1(i: &[isize], rounds: usize) -> isize {
  mix(i, rounds)
}

fn solve_p2(i: &[isize], rounds: usize, key: isize) -> isize {
  let ni: Vec<isize> = i.iter().map(|e| *e * key).collect();

  mix(&ni, rounds)
}

fn main() {
  for input_file in ["example", "input"] {
    let input = read_to_string(input_file).expect("input");

    let ns = parse(&input);

    println!("{}: P1 ans: {:?}", input_file, solve_p1(&ns, 1));
    println!("{}: P2 ans: {:?}", input_file, solve_p2(&ns, 10, 811589153));
  }
}
