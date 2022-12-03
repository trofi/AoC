use std::collections::HashSet;
use std::fs::File;
use std::io::BufRead;
use std::io::BufReader;

fn solve(input: &Vec<String>) -> usize {
  input.chunks(3).map(|c| {
      let dupes: Vec<_> =
          c.iter().map(|s| s.chars().collect())
           .reduce(|r: HashSet<_>, s| r.intersection(&s).cloned().collect())
           .expect("at least one value in chunks of 3")
           .iter()
           .cloned()
           .collect();
      match dupes.as_slice() {
        // TODO: is there a nicer way to both destructure the slice,
        // bind it's value and validate it's contents for value range like &['a'..='z'?]
        &[v] if v >= 'a' && v <= 'z' => 1  + (v as usize - 'a' as usize),
        &[v] if v >= 'A' && v <= 'Z' => 27 + (v as usize - 'A' as usize),
        v                         => panic!("unexpected dupes {:?}", v)
      }
    }
  ).sum()
}

fn read_input(input_file: &str) -> Vec<String> {
  let f = File::open(input_file).unwrap();
  let bf = BufReader::new(f);

  bf.lines()
    .map(|l| l.expect("expect line"))
    .collect()
}

fn main() {
  for ifile in ["example" , "input"] {
    let input = read_input(ifile);
    let ans = solve(&input);
    println!("{}: P1 ans: {:?}", ifile, ans);
  }
}
