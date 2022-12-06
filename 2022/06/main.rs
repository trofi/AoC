use std::fs::read_to_string;
use std::error::Error;

type E = Box<dyn Error>;

fn all_unique(i: &[char]) -> bool {
  let mut v = Vec::from(i);
  v.sort();

  v.iter()
   .zip(v.iter()
         .skip(1)).all(|(a, b)| a != b)
}

fn solve(i: &Vec<&str>, l: usize) -> Vec<usize> {
  i.iter().map(|e|
    e.chars()
     .collect::<Vec<_>>()
     .windows(l)
     .enumerate()
     .find(|(_, e)| all_unique(*e))
     .expect("has unique sequence")
     .0 + l
  ).collect()
}

fn main() -> Result<(), E> {
  for ifile in ["example" , "input"] {
    let input_string = read_to_string(ifile)?;
    let input = input_string.lines().collect();

    let ans_p1 = solve(&input, 4);
    println!("{}: P1 ans: {:?}", ifile, ans_p1);
    let ans_p2 = solve(&input, 14);
    println!("{}: P2 ans: {:?}", ifile, ans_p2);
  }
  Ok(())
}
