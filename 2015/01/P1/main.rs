use std::fs::read_to_string;

fn solve(i: &Vec<&str>) -> Vec<isize> {
  i.iter().map(|l|
    l.chars().map(|c| match c {
      '(' => 1,
      ')' => -1,
      _   => panic!("Unknown {:?} symbol", c)
    }).sum()
  ).collect()
}

fn main() {
  for input_file in ["example", "input"] {
    let i = read_to_string(input_file).expect("all ok");
    let is = i.lines().collect();
    let ans = solve(&is);
    println!("ans: {:?}", ans);
  }
}
