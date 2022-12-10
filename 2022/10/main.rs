use std::fs::read_to_string;
use std::error::Error;
use std::collections::HashMap;

type E = Box<dyn Error>;

struct Cpu {
  x: isize,
  cyc: isize,

  result: isize, /* p1 */
  crt: HashMap<(isize, isize), char>, /* p2 */
}

impl Cpu {
  fn new() -> Self {
    Cpu{
      x: 1,
      cyc: 0,
      result: 0,
      crt: HashMap::new(),
    }
  }

  fn crt_strobe(self: &mut Self) {
    let h = self.cyc % 40;
    let v = self.cyc / 40;
    let c = if self.x >= h - 1 && self.x <= h + 1 { '#' } else { '.' };
    self.crt.insert((h,v), c);
  }

  fn tick(self: &mut Self) {
    self.crt_strobe();

    self.cyc += 1;
    if self.cyc % 40 == 20 {
      self.result += self.x * self.cyc;
    };

    self.crt_strobe();
  }
  fn print_crt(self: &Self) {
    for r in 0..6 {
      for c in 0..40 {
        let ch = self.crt.get(&(c,r)).unwrap_or(&'?');
        print!("{}", ch);
      }
      println!();
    }
  }

  fn noop(self: &mut Self) {
    self.tick();
  }
  fn addx(self: &mut Self, v: isize) {
    self.tick();
    self.tick();
    self.x += v;
  }
}

fn solve(i: &str) -> Cpu{
  let mut cpu = Cpu::new();

  for l in i.lines() {
    let v: Vec<_> = l.split(' ').collect();
    match v.as_slice() {
      ["noop"]    => { cpu.noop(); },
      ["addx", v] => { cpu.addx(v.parse::<isize>().unwrap()); },
      _           => panic!("Unknown command: '{}'", l),
    }
  }

  cpu
}

fn main() -> Result<(), E> {
  for ifile in ["example" , "input"] {
    let input = read_to_string(ifile)?;

    let cpu = solve(&input);

    println!("{}: P1 ans: {:?}", ifile, cpu.result);
    println!("{}: P2 ans", ifile);
    cpu.print_crt();
  }
  Ok(())
}
