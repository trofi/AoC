use std::fs::read_to_string;
use std::collections::BTreeSet;
use std::collections::HashMap;
use std::collections::HashSet;
use std::collections::VecDeque;
use std::cmp::max;

fn parse(i: &str) -> Vec<char> {
  i.chars().collect()
}

type Pos = (isize, isize);
type Field = HashSet<Pos>;
type Piece = Vec<Pos>;

const WELL_WIDTH: isize = 7;

// Does our point collide with anything in the well?
fn is_coll(f: &Field, pos@(x, _): &Pos) -> bool {
  f.contains(pos) || *x < 0 || *x >= WELL_WIDTH
}

#[derive(Eq, Hash, PartialEq)]
struct Profile {
  piece: usize,
  jet_ix: usize,
  free: BTreeSet<Pos>,
}

// Build profile of our well. Everything that is reachable from the top
// is in profile. Use simple BFS. Example:
//
// |.......#| <- h
// |.#####.#|
// |#.######|
fn fetch_profile(f: &Field, h: isize) -> BTreeSet<Pos> {
  let mut q: VecDeque<Pos> = VecDeque::new();
  let mut visited: HashSet<Pos> = HashSet::new();
  for x in 0..WELL_WIDTH {
    if !is_coll(f, &(x, h)) {
      q.push_back((x, h));
    }
  }

  while !q.is_empty() {
    let pos@(x, y) = q.pop_front().unwrap();
    if visited.contains(&pos) { continue }
    visited.insert(pos);

    for npos in [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)] {
      if npos.1 < h && !is_coll(f, &npos) {
        q.push_back(npos);
      }
    }
  }

  // normalize against 'h'
  visited.iter().map(|(x, y)| (*x, *y - h)).collect()
}

fn solve(cmds: &Vec<char>, rounds: usize) -> isize {
  // offsets encoded from left-bottom being 0,0
  let pieces: Vec<Piece> = vec![
    vec![(0, 0), (1, 0), (2, 0), (3, 0)], // minus
    vec![(1, 0), (0, 1), (1, 1), (2, 1), (1, 2)], // cross
    vec![(0, 0), (1, 0), (2, 0), (2, 1), (2, 2)], // corner
    vec![(0, 0), (0, 1), (0, 2), (0, 3)], // I
    vec![(0, 0), (0, 1), (1, 0), (1, 1)], // square
  ];

  let mut field: Field = Field::new();

  let mut piece = 0;
  let mut jet_ix = 0;
  let mut h = -1;
  let mut h_skipped = 0;
  // add bottom
  for x in 0..WELL_WIDTH {
    field.insert((x, h));
  }

  let mut known_profiles: HashMap<Profile, (isize, usize)> = HashMap::new();

  let mut round = 0;
  let mut mrounds = rounds;
  loop {
    if round >= mrounds { break }
    round += 1;

    // Check first if we already saw this pattern before.
    // if we did instead of simulation let's skip entirely
    // the simulation and directly bump the height.
    let profile = Profile {
      piece: piece,
      jet_ix: jet_ix,
      free: fetch_profile(&field, h),
    };
    match known_profiles.get(&profile) {
      None           => { known_profiles.insert(profile, (h, round)); },
      Some((ph, pr)) => {
        // Found duplicate profile can cut down rest of rounds by a modulo
        let loop_rounds = round - pr;
        let rounds_left = mrounds - round;
        let full_iterations = rounds_left / loop_rounds;

        h_skipped += (h - ph) * (full_iterations as isize);
        mrounds -= loop_rounds * full_iterations;
      }
    }

    // Perform piece fall simulation.
    let mut p: Piece =
      pieces[piece]
        .iter()
        .map(|(x, y)| (2 + x, h + 4 + y))
        .collect();
    piece = (piece + 1) % pieces.len();

    loop {
      // jet piece sideways
      let mp: Piece =
        p.iter()
         .map(|(x, y)| match cmds[jet_ix] {
           '>' => (*x + 1, *y),
           '<' => (*x - 1, *y),
           c   => panic!("Unhandled {} command", c)
         })
         .collect();
      jet_ix = (jet_ix + 1) % cmds.len();

      if mp.iter().all(|p| !is_coll(&field, &p)) {
        p = mp
      }

      // move piece down
      let mp: Piece =
        p.iter()
         .map(|(x, y)| (*x, *y - 1))
         .collect();

      if mp.iter().all(|p| !is_coll(&field, &p)) {
        p = mp;
        continue
      }

      // solidify the piece by persisting in into the field
      for pos in p.iter() {
        field.insert(*pos);
      }
      h = max(h, *p.iter().map(|(_, y)| y).max().unwrap());

      break
    }
  }

  h + 1 + h_skipped
}

fn main() {
  for input_file in ["example", "input"] {
    let input = read_to_string(input_file).expect("input");

    let cmds = parse(&input);

    let ans_p1 = solve(&cmds, 2022);
    println!("{}: P1 ans: {:?}", input_file, ans_p1);

    let ans_p2 = solve(&cmds, 1000000000000);
    println!("{}: P2 ans: {:?}", input_file, ans_p2);
  }
}
