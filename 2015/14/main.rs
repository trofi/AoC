struct Spec {
  speed: usize,
  stamina: usize,
  cooldown: usize,
}

fn parse(i: &str) -> Vec<Spec> {
  i.lines().map(|l| {
    let v: Vec<_> = l.split(' ').collect();
    match v.as_slice() {
      [_, "can", "fly", sspeed, "km/s",
       "for", sstamina, "seconds,", "but", "then", "must", "rest", "for",
       scooldown, "seconds."] => Spec {
         speed: sspeed.parse().unwrap(),
         stamina: sstamina.parse().unwrap(),
         cooldown: scooldown.parse().unwrap(),
       },
      _ => panic!("Unhandled input: {:?}", v)
    }
  }).collect()
}

fn travel(s: &Spec, t: usize) -> usize {
  let period = s.stamina + s.cooldown;
  let full_periods = t / period;
  let t_left = t - full_periods * period;

  s.speed * (s.stamina * full_periods + std::cmp::min(s.stamina, t_left))
}

fn solve_p1(deers: &[Spec], t: usize) -> usize {
  deers.iter().map(|s| travel(s, t)).max().unwrap()
}

fn solve_p2(deers: &[Spec], t: usize) -> usize {
  let mut scoreboard: Vec<usize> = deers.iter().map(|_| 0).collect();

  for round in 1..=t {
    let best = solve_p1(deers, round);
    for i in 0..deers.len() {
      if solve_p1(&deers[i..i+1], round) == best {
        scoreboard[i] += 1;
      }
    }
  }

  *scoreboard.iter().max().unwrap()
}

fn main() {
  for input_file in ["example", "input"] {
    let i = std::fs::read_to_string(input_file).expect("all ok");

    let specs = parse(&i);

    println!("{}: P1 ans: {:?}", input_file, solve_p1(&specs, 2503));
    println!("{}: P2 ans: {:?}", input_file, solve_p2(&specs, 2503));
  }
}
