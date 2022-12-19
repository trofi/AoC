use std::fs::read_to_string;
use std::collections::VecDeque;
use std::collections::BTreeSet;

#[derive(Clone, Debug, Eq, PartialEq, Ord, PartialOrd)]
struct Res {
  ore:      usize,
  clay:     usize,
  obsidian: usize,
  geode:    usize,
}

impl Res {
  fn add(self: &Self, other: &Self) -> Self {
    Res{
      ore:      self.ore      + other.ore,
      clay:     self.clay     + other.clay,
      obsidian: self.obsidian + other.obsidian,
      geode:    self.geode    + other.geode,
    }
  }
  fn sub(self: &Self, other: &Self) -> Option<Self> {
    if self.ore      < other.ore      { return None; }
    if self.clay     < other.clay     { return None; }
    if self.obsidian < other.obsidian { return None; }
    if self.geode    < other.geode    { return None; }

    Some(Res{
      ore:      self.ore      - other.ore,
      clay:     self.clay     - other.clay,
      obsidian: self.obsidian - other.obsidian,
      geode:    self.geode    - other.geode,
    })
  }
}

#[derive(Debug)]
struct BP {
  ix: usize,
  robots: Vec<(Res, Res)>, // (production yield, construction cost)
}

fn s2u(s: &str) -> usize {
  s.parse().expect("expect usize")
}

fn parse(i: &str) -> Vec<BP> {
  i.lines().map(|l| {
    let vl: Vec<_> = l.split(&[':', ' ', '.']).collect();
    match vl.as_slice() {
      ["Blueprint", ix,
       "", "Each", "ore", "robot", "costs", o_o, "ore",
       "", "Each", "clay", "robot", "costs", c_o, "ore",
       "", "Each", "obsidian", "robot", "costs", ob_o, "ore", "and", ob_c, "clay",
       "", "Each", "geode", "robot", "costs", g_o, "ore", "and", g_ob, "obsidian", ""] =>
        BP {
          ix: s2u(ix),
          robots: vec![
            ( Res{ore: 1,         clay: 0,         obsidian: 0,         geode: 0}
            , Res{ore: s2u(o_o),  clay: 0,         obsidian: 0,         geode: 0}
            ),
            ( Res{ore: 0,         clay: 1,         obsidian: 0,         geode: 0}
            , Res{ore: s2u(c_o),  clay: 0,         obsidian: 0,         geode: 0}
            ),
            ( Res{ore: 0,         clay: 0,         obsidian: 1,         geode: 0}
            , Res{ore: s2u(ob_o), clay: s2u(ob_c), obsidian: 0,         geode: 0}
            ),
            ( Res{ore: 0,         clay: 0,         obsidian: 0,         geode: 1}
            , Res{ore: s2u(g_o),  clay: 0,         obsidian: s2u(g_ob), geode: 0}
            ),
          ],
        },
      _ => panic!("Can't handle {:?} input", vl)
    }
  }).collect()
}

#[derive(Debug, Ord, Eq, PartialEq, PartialOrd, Clone)]
struct State {
  time_left: usize,
  res: Res,
  res_per_day: Res,
}

fn solve(i: &[BP], time: usize) -> Vec<(usize, usize)> {
  i.iter().map(|bp| {
    let max_req = Res {
      ore:      bp.robots.iter().map(|(_, e)| e.ore     ).max().expect("ore"),
      clay:     bp.robots.iter().map(|(_, e)| e.clay    ).max().expect("clay"),
      obsidian: bp.robots.iter().map(|(_, e)| e.obsidian).max().expect("obsidian"),
      geode:    bp.robots.iter().map(|(_, e)| e.geode   ).max().expect("geode"),
    };

    let mut q: VecDeque<State> = VecDeque::new();
    let s = State{
      time_left:   time,
      res:         Res{ore: 0, clay: 0, obsidian: 0, geode: 0},
      res_per_day: Res{ore: 1, clay: 0, obsidian: 0, geode: 0},
    };
    q.push_back(s);

    let mut r = 0;

    let mut visited: BTreeSet<State> = BTreeSet::new();

    while !q.is_empty() {
      let s = q.pop_back().unwrap();

      if visited.contains(&s) { continue }
      visited.insert(s.clone());

      if s.time_left == 0 {
        if s.res.geode > r {
          r = s.res.geode;
        }
        continue
      }

      // check if even best case (produce obsidian bots on each move)
      // can't do any better.
      {
        let best_r =
            s.res.geode
            + s.time_left * s.res_per_day.geode
            + s.time_left * (s.time_left - 1) / 2;
        if best_r <= r { continue }
      }

      // add resources without robot construction
      {
        let ns = State{
          time_left:   s.time_left - 1,
          res:         s.res.add(&s.res_per_day),
          res_per_day: s.res_per_day.clone(),
        };
        q.push_back(ns);
      }

      // add resource with 1 robot construction, if feasible
      for (prod, cost) in &bp.robots {
        // Skip case when producing intermediate resources does not
        // contribute to creation of new bots.
        if    (prod.ore      > 0 && s.res_per_day.ore      >= max_req.ore)
           || (prod.clay     > 0 && s.res_per_day.clay     >= max_req.clay)
           || (prod.obsidian > 0 && s.res_per_day.obsidian >= max_req.obsidian)
        { continue }

        if let Some(res_left) = s.res.sub(&cost) {
          let ns = State{
            time_left:   s.time_left - 1,
            res:         res_left.add(&s.res_per_day),
            res_per_day: s.res_per_day.add(&prod),
          };
          q.push_back(ns);
        }
      }
    }

    (r, bp.ix)
  }).collect()
}

fn main() {
  for input_file in ["example", "input"] {
    let input = read_to_string(input_file).expect("input");

    let blueprints = parse(&input);

    println!("{}: P1 ans: {:?}",
             input_file,
             solve(&blueprints, 24)
                 .iter()
                 .map(|(r, bix)| r*bix)
                 .sum::<usize>());
    println!("{}: P2 ans: {:?}",
             input_file,
             solve(&blueprints[0..3], 32)
                 .iter()
                 .map(|(r, _)| r)
                 .product::<usize>()
             );
  }
}
