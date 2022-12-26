use std::collections::HashMap;

fn parse(s: &str) -> Vec<usize> {
  s.lines()
   .map(|e| e.parse()
             .expect("digit"))
   .collect()
}

fn solve_p1(is: &[usize], v: usize) -> usize {
  // vol=>count
  let mut c: HashMap<usize, usize> = HashMap::new();

  for new_vol in is {
    for (vol, cnt) in c.clone() {
      c.entry(vol + new_vol)
       .and_modify(|num| *num += cnt)
       .or_insert(cnt);
    }
    c.entry(*new_vol)
     .and_modify(|num| *num += 1)
     .or_insert(1);
  }

  *c.get(&v).unwrap()
}

fn solve_p2(is: &[usize], v: usize) -> usize {
  // vol=>count
  let mut c: HashMap<(usize, usize), usize> = HashMap::new();

  for new_vol in is {
    for ((vol, cc), cnt) in c.clone() {
      c.entry((vol + new_vol, cc + 1))
       .and_modify(|num| *num += cnt)
       .or_insert(cnt);
    }
    c.entry((*new_vol, 1))
     .and_modify(|num| *num += 1)
     .or_insert(1);
  }

  *c.iter().filter(|((vol, _), _)| *vol == v).min().unwrap().1
}

fn main() {
  for (vol, input_file) in [(25, "example"), (150, "input")] {
    let i = std::fs::read_to_string(input_file).expect("all ok");

    let specs = parse(&i);

    println!("{}: P1 ans: {:?}", input_file, solve_p1(&specs, vol));
    println!("{}: P1 ans: {:?}", input_file, solve_p2(&specs, vol));
  }
}
