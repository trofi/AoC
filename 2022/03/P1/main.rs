use std::collections::HashSet;
use std::fs::File;
use std::io::BufRead;
use std::io::BufReader;

fn solve(input: &Vec<String>) -> usize {
  input.iter().map(|l| {
      let vl: Vec<_> = l.chars().collect();

      let (l, r) = vl.split_at(vl.len() / 2);

      // TODO: have to use cloned() a lot to keep copying
      // chars by value instead of building containers of
      // references on references. Is there a more implicit
      // way to do it?
      let ls : HashSet<_> = l.iter().cloned().collect();
      let rs : HashSet<_> = r.iter().cloned().collect();

      let dupes: Vec<_> = ls.intersection(&rs).cloned().collect();

      match dupes.as_slice() {
        &[v] if v >= 'a' && v <= 'z' => 1 + (v as usize - 'a' as usize),
        &[v] if v >= 'A' && v <= 'Z' => 27 + (v as usize - 'A' as usize),
        v                            => panic!("unexpected dupe {:?}", v)
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
