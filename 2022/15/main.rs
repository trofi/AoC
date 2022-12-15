use std::fs::read_to_string;
use std::collections::HashSet;

type Pos = (isize, isize);
type SB  = (Pos, Pos);

fn parse(i: &str) -> Vec<SB> {
  i.lines().map(|l|{
    // "Sensor at x=2, y=18: closest beacon is at x=-2, y=15"
    let v: Vec<_> = l.split(&[' ', '=', ',', ':']).collect();
    match v.as_slice() {
      ["Sensor", "at", "x", sx, "", "y", sy, "",
       "closest", "beacon", "is", "at", "x", bx, "", "y", by] => {
        ((sx.parse().unwrap(), sy.parse().unwrap()),
         (bx.parse().unwrap(), by.parse().unwrap()))
      },
      _ => panic!("Unexpected {:?} input", v)
    }
  }).collect()
}

fn solve_p1(i: &Vec<SB>, y: isize) -> usize {
  // collect reachable sensors that touch 'y'
  // and store their coverage ranges on 'y' axis as (from, to) pair.
  let mut rs: Vec<(isize, isize)> = Vec::new();
  for ((sx, sy), (bx, by)) in i {
    let d = (bx - sx).abs() + (by - sy).abs();
    let y_distance = (sy - y).abs();
    if y_distance <= d {
      rs.push((sx - (d - y_distance), sx + (d - y_distance)));
    }
  }
  rs.sort();

  let mut res: usize  = 0;
  let mut x: isize = rs[0].0 - 1; // rightmost interval included in 'r'

  // calculate all intervals
  for (l, r) in &rs {
    // ..x] .. [l..r]
    if x < *l {
      res += (r - l + 1) as usize;
      x = *r;
    // [l .. x .. r]
    } else if x <= *r {
      res += (r - x) as usize;
      x = *r;
    // // [ l .. r] ... [x
    } else if *r < x {
      /* just skip */
    } else {
      panic!("Unhandled overlap {:?}", (l, r));
    }
  }

  // remove known beacons in intervals
  let bs: HashSet<&Pos> = i.iter().map(|(_, b)| b).collect();
  res -= bs.iter().filter(|(_, by)| *by == y).count();

  res
}

// Similar to p1: scan each 'y' for unknown range. We expect
// everything except one dot to be covered. If there is just
// one unknown then it must be our case.
fn solve_p2(i: &Vec<SB>, l: isize) -> isize {
  let mut candidates: Vec<isize> = Vec::new();

  for y in 0..l {
    let mut rs: Vec<(isize, isize)> = Vec::new();
    for ((sx, sy), (bx, by)) in i {
      let d = (bx - sx).abs() + (by - sy).abs();
      let y_distance = (sy - y).abs();
      if y_distance <= d {
        rs.push((sx - (d - y_distance), sx + (d - y_distance)));
      }
    }
    rs.sort();

    let mut x: isize = -1; // rightmost already scanned
    // calculate all intervals
    for (l, r) in &rs {
      // ..x] .. [l..r]
      if x < *l {
        // ..x] [!] [l..r]
        if *l - x == 2 {
          println!("FOUND candidate {:?}", (x + 1, y));
          candidates.push(4000000 * (x + 1) + y);
        }
        x = *r;
      // [l .. x .. r]
      } else if x <= *r {
        x = *r;
      // // [ l .. r] ... [x
      } else if *r < x {
        /* just skip */
      } else {
        panic!("Unhandled overlap {:?}", (l, r));
      }
    }
  }

  // in case we don't handle corners properly
  if candidates.len() != 1 { todo!(); }
  candidates[0]
}

fn main() {
  for (y, l, input_file) in [ (10,      20,      "example")
                            , (2000000, 4000000, "input")] {
    let input = read_to_string(input_file).expect("input");

    let readings = parse(&input);

    let ans_p1 = solve_p1(&readings, y);
    println!("{}: P1 ans: {:?}", input_file, ans_p1);
    let ans_p2 = solve_p2(&readings, l);
    println!("{}: P2 ans: {:?}", input_file, ans_p2);
  }
}
