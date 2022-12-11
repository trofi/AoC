use std::fs::read_to_string;
use std::collections::HashMap;

fn solve_p1(i: &str) -> usize {
  let all_vowels = vec!['a', 'e', 'i', 'o', 'u'];
  let naughty: Vec<&[char]> = vec![&['a', 'b'], &['c', 'd'], &['p', 'q'], &['x', 'y']];

  let is_nice = |s: &str| -> bool {
    let vs: Vec<_> = s.chars().collect();

    let vowels =
        vs.iter()
          .filter(|e| all_vowels.contains(e))
          .count();
    let doubles =
        vs.windows(2)
          .filter(|p| p[0] == p[1])
          .count();

    let naugthies =
        vs.windows(2)
          .filter(|p| naughty.contains(p))
          .count();

    vowels >= 3 && doubles > 0 && naugthies == 0
  };

  i.lines().filter(|l| is_nice(l)).count()
}

fn solve_p2(i: &str) -> usize {
  let is_nice = |s: &str| -> bool {
    let vs: Vec<_> = s.chars().collect();

    let mut has_pair = false;
    let mut seen: HashMap<&[char], usize> = HashMap::new();

    for (i, ch) in vs.windows(2).enumerate() {
      match seen.get(ch) {
        None => { seen.insert(ch, i); },
        Some(ix) => { has_pair |= i - *ix > 1; },
      }
    }

    let ts =
        vs.windows(3)
          .filter(|p| p[0] == p[2])
          .count();

    has_pair && ts > 0
  };

  i.lines().filter(|l| is_nice(l)).count()
}

fn main() {
  for input_file in ["example", "example2", "input"] {
    let i = read_to_string(input_file).expect("all ok");

    let ans_p1 = solve_p1(&i);
    println!("{}: P1 ans: {:?}", input_file, ans_p1);
    // not 82, not 45, not 68, not 51
    let ans_p2 = solve_p2(&i);
    println!("{}: P2 ans: {:?}", input_file, ans_p2);
  }
}
