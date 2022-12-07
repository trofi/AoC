use std::fs::read_to_string;

fn solve(i: &str) -> usize {
  let mut f = 0;
  for (ix, c) in i.chars().enumerate() {
    f += match c {
      '(' => 1,
      ')' => -1,
      _   => panic!("Unknown {:?} symbol", c)
    };
    if f == -1 {
      return ix + 1;
    }
  }
  panic!("Never reached -1 :(")
}

fn main() {
  for input_file in ["example", "input"] {
    let i = read_to_string(input_file).expect("all ok");
    let ans = solve(&i);
    println!("ans: {:?}", ans);
  }
}
