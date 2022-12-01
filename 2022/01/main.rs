use std::fs::File;
use std::io::BufRead;
use std::io::BufReader;

fn read_input(input_file: &str) -> Vec<Vec<usize>> {
  let f = File::open(input_file).unwrap();
  let bf = BufReader::new(f);

  let mut r = Vec::new();
  r.push(Vec::new());

  // Is there a split() form for Iterator?
  bf.lines().fold(&mut r, |s, e| {
    let v = e.unwrap();
    if v == "" {
      s.push(Vec::new());
    } else {
      let last = s.len() - 1;
      s[last].push(v.parse().unwrap());
    }
    s
  });

  r
}

fn main() {
    for ifile in ["example", "input"] {
        let input = read_input(ifile);
        let mut ans: Vec<usize> = input.iter()
                                       .map(|e| e.iter().sum())
                                       .collect();
        ans.sort();
        println!("{}: max:  {:?}", ifile, ans.iter().max().unwrap());
        println!("{}: max3: {:?}", ifile, ans.iter().rev().take(3).sum::<usize>());
    }
}
