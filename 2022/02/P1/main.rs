use std::cmp::Ord;
use std::cmp::Ordering;
use std::fs::File;
use std::io::BufRead;
use std::io::BufReader;
use std::iter::FromIterator;
use std::str::FromStr;

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
enum V { R, P, S }

impl V {
  fn val(self: V) -> usize {
    match self { V::R => 1, V::P => 2, V::S => 3 }
  }
}

impl Ord for V {
  fn cmp(&self, other: &Self) -> Ordering {
    match (self, other) {
      (a, b) if a == b => Ordering::Equal,
      (V::R, V::P) | (V::P, V::S) | (V::S, V::R) => Ordering::Less,
      _ => Ordering::Greater
    }
  }
}

// TODO: can we implement Ord without PartialOrd? Or derive
// PartialOrd out of Ord?
impl PartialOrd for V {
  fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
    Some(self.cmp(other))
  }
}

#[derive(Debug)]
struct Round {
  them: V,
  me: V,
}

impl Round {
  fn val(self: &Round) -> usize {
      3 * (match self.me.cmp(&self.them) {
          Ordering::Less => 0,
          Ordering::Equal => 1,
          Ordering::Greater => 2
      }) + self.me.val()
  }
}

impl FromStr for Round {
  type Err = ();
  fn from_str(s: &str) -> Result<Self, Self::Err> {
    let v = Vec::from_iter(s.chars());
    match v.as_slice() {
      // "<THEM> <ME>"
      [t, ' ', m] => Ok(Round {
        them: match t {
          'A' => V::R, 'B' => V::P, 'C' => V::S, _ => panic!("Uknown 'them' round: {}", t)
        },
        me: match m {
          'X' => V::R, 'Y' => V::P, 'Z' => V::S, _ => panic!("Uknown 'me' round: {}", t)
        },
      }),
      _ => panic!("Uknown input string: {}", s)
    }
  }
}

fn read_input(input_file: &str) -> Vec<Round> {
  let f = File::open(input_file).unwrap();
  let bf = BufReader::new(f);

  bf.lines().map(|l| l.expect("line")
                      .parse().expect("round"))
            .collect()
}

fn main() {
  for ifile in ["example" , "input"] {
    let input = read_input(ifile);
    let ans: usize = input.iter()
                          .map(|r| r.val())
                          .sum();
    println!("{}: ans: {:?}", ifile, ans);
  }
}
