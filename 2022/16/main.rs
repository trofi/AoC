use std::collections::BinaryHeap;
use std::collections::BTreeSet;
use std::collections::HashMap;
use std::fs::read_to_string;

type NodeId = String;

struct Node {
  val:   usize,
  edges: BTreeSet<NodeId>,
}

fn parse(i: &str) -> HashMap<NodeId, Node> {
  i.lines().map(|l|{
    let vl: Vec<_> = l.split("; ").collect();
    assert!(vl.len() == 2);

    let vnv: Vec<_> = vl[0].split(&[' ', '=']).collect();
    let (name, val) = match vnv.as_slice() {
      ["Valve", n, "has", "flow", "rate", r] => (n, r.parse().expect("number")),
      _ => panic!("Unknown {:?} node description", vnv)
    };

    let edges = if let Some(el) = vl[1].strip_prefix("tunnels lead to valves ") {
      el.split(", ").map(|e| e.to_string()).collect()
    } else if let Some(el) = vl[1].strip_prefix("tunnel leads to valve ") {
      el.split(", ").map(|e| e.to_string()).collect()
    } else {
      panic!("Unknown {:?} edge description", vl[1])
    };

    (name.to_string(),
     Node {
       val: val,
       edges: edges,
     })
  }).collect()
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Hash, Clone, Debug)]
struct StateP2<'a> {
  prio: usize, /* first goes natural priority key */
  val: usize,
  fuel_left: usize,
  currents: Vec<&'a String>,
  who_moves: usize,
  opened: BTreeSet<&'a String>,
}

// A* search where priority is an outcome
// if each step would open best valve.
fn solve_p2(i: &HashMap<String, Node>, count: usize, fuel: usize) -> usize {
  // estimate node priority to consider first
  let best_pending_outcome = |s: &StateP2| {
    let mut v: Vec<_> =
        i.iter()
         .filter(|(k,_)| !s.opened.contains(k))
         .map(|(_,v)| v)
         .collect();
    v.sort_by_key(|e| e.val);

    let mut r = s.val;
    let mut f = s.fuel_left;
    // best case: we unlock biggest left valves
    for n in v.iter().rev() {
      if f == 0 { break }
      r += f * n.val;
      f -= 1;
      if f == 0 { break }
      f -= 1;
      if f == 0 { break }
    }

    r
  };
  let mut q: BinaryHeap<StateP2> = BinaryHeap::new();

  let mut initial = StateP2{
    prio:      0,
    val:       0,
    fuel_left: fuel,
    currents:  Vec::new(),
    who_moves: 0,
    opened:    BTreeSet::new(), // TODO: bitvector might be more compact
  };
  let inode = "AA".to_string();
  initial.prio = best_pending_outcome(&initial);
  initial.currents.resize(count, &inode);

  q.push(initial);

  let mut best = 0;

  while !q.is_empty() {
    let s = q.pop().unwrap();

    if s.who_moves == 0 && s.val > best {
      best = s.val;
    }

    if s.who_moves == 0 && s.fuel_left == 0 { continue }

    let n = i.get(s.currents[s.who_moves]).expect("node info");
    let who_moves_next = (s.who_moves + 1) % count;
    let fuel_reduction = if who_moves_next == 0 { 1 } else { 0 };

    // search space: handle open, skip 0-valves
    if !s.opened.contains(s.currents[s.who_moves]) && n.val > 0 {
      let mut ns = s.clone();
      ns.val += n.val * (ns.fuel_left - 1);
      ns.fuel_left -= fuel_reduction;
      ns.who_moves = who_moves_next;
      ns.opened.insert(s.currents[s.who_moves]);
      ns.prio = best_pending_outcome(&ns);
      if ns.prio > best {
        q.push(ns);
      }
    }

    // search cpace: handle moves
    for e in n.edges.iter() {
      let mut ns = s.clone();
      ns.fuel_left -= fuel_reduction;
      ns.who_moves = who_moves_next;
      ns.currents[s.who_moves] = e;
      ns.prio = best_pending_outcome(&ns);
      if ns.prio > best {
        q.push(ns);
      }
    }
  }

  best
}

fn main() {
  for input_file in ["example", "input"] {
    let input = read_to_string(input_file).expect("input");

    let i = parse(&input);

    let ans_p1 = solve_p2(&i, 1, 30);
    println!("{}: P1 ans: {:?}", input_file, ans_p1);

    let ans_p2 = solve_p2(&i, 2, 26);
    println!("{}: P2 ans: {:?}", input_file, ans_p2);
  }
}
