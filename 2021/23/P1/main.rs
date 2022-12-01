/*
A - 1
B - 10
C - 100
D - 1000

Constraints:
- no stop immediately on room entrance
- move from entrance to the destined room only, no foreign rooms, no foreign presence
- move from entrance only if it can immediately reach the room

example:

#############
#...........#
###B#C#B#D###
  #A#D#C#A#
  #########

input:

#############
#...........#
###B#B#C#D###
  #D#C#A#A#
  #########

*/

use std::{cmp, collections::*, iter::*};

#[derive(Debug,Eq,PartialEq)]
enum SqType {
    Hallway(bool), // true: can stop here.
    Room(char), // char is type
}
use SqType::*;

type Pos = (isize, isize); // (col, row)

type Map = BTreeMap<Pos, SqType>;

fn mk_map() -> Map {
    let mut m = Map::new();

    for col in 0..=10 {
        m.insert((col, 0), Hallway(true));
    }
    for (col, t) in &[(2, 'A'), (4, 'B'), (6, 'C'), (8, 'D')] {
        m.insert((*col, 0), Hallway(false));
        m.insert((*col, 1), Room(*t));
        m.insert((*col, 2), Room(*t));
    }

    return m;
}

#[derive(Clone,Eq,PartialEq,PartialOrd,Ord)]
struct State {
    pos: BTreeMap<Pos, char>,
}

fn mk_state(initial: &[&str]) -> State {
    let mut pos = BTreeMap::new();

    for (row, s) in initial.iter().enumerate() {
        for (col, c) in s.chars().enumerate() {
            pos.insert((2 + 2 * col as isize, 1 + row as isize), c);
        }
    }

    return State{
        pos,
    };
}

fn show(m: &Map, s: &State, cost: usize) {
    println!("");
    println!("Took cost: {}", cost);
    for row in -1..=3 {
        let s = String::from_iter(
            (-1..=11).map(|col|
                match (s.pos.get(&(col, row)), m.get(&(col, row)))  {
                    (Some(c), _) => *c,
                    (_, Some(t)) => match t {
                        Hallway(true)  => '.',
                        Hallway(false) => '*',
                        Room(c) => c.to_lowercase().next().unwrap(),
                    },
                    (None, None) => '#',
                }
            )
        );
        println!("{}", s);
    }
}

fn is_final(m: &Map, s: &State) -> bool {
    // are all these shrimps on their places?
    return s.pos.iter().all(|(p, c)| m.get(p) == Some(&Room(*c)));
}

fn move_cost(c: char) -> usize {
    match c {
        'A' => 1, 'B' => 10, 'C' => 100, 'D' => 1000,
        _ => unreachable!("Unexpected type {:?}", c),
    }
}

fn possible_moves(m: &Map, s: &State) -> Vec<(usize, State)> {
    let mut r = Vec::new();

    for (pos, c) in s.pos.iter() {
        match m.get(pos) {
            // Hallway -> Room (type matches)
            Some(Hallway(true)) => {
                let mut possible_dests = BTreeSet::<Pos>::from_iter(
                    m.iter()
                     .filter(|(_dp, ds)|**ds == Room(*c))
                     .map(|e| *e.0));

                for (opos, oc) in s.pos.iter() {
                    if possible_dests.contains(opos) {
                        if oc == c {
                            possible_dests.remove(opos);
                        } else {
                            possible_dests.clear();
                        }
                    }
                }

                let pd = match possible_dests.iter().max() {
                    Some(v) => v,
                    None => continue,
                };

                let mut path = Vec::new();
                for col in cmp::min(pd.0, pos.0)..=cmp::max(pd.0, pos.0) {
                    if (col,0) != *pos { // skip current pos
                        path.push((col, 0));
                    }
                }
                for row in 1..=pd.1 {
                    path.push((pd.0, row));
                }

                let mc = path.len() * move_cost(*c);
                if path.iter().all(|p| s.pos.get(p) == None) {
                    let mut new_s_pos = s.pos.clone();
                    new_s_pos.remove(pos);
                    new_s_pos.insert(*pd, *c);
                    //println!("consider {} move: {:?} -> {:?}; cost={}l path={:?}", c, pos, pd, mc, path);
                    r.push((mc, State{pos: new_s_pos}));
                } else {
                    //println!("rejected {} move: {:?} -> {:?}; cost={}; path={:?}", c, pos, pd, mc, path);
                }
            },
            // Room -> Hallway(true)
            Some(Room(_)) => {
                let possible_dests : Vec<Pos> = Vec::from_iter(
                    m.iter()
                     .filter(|(_dp, ds)|**ds == Hallway(true))
                     .map(|e| *e.0));
                for pd in &possible_dests {
                    let mut path = Vec::new();
                    for col in cmp::min(pd.0, pos.0)..=cmp::max(pd.0, pos.0) {
                       path.push((col, 0));
                    }
                    for row in 1..pos.1 {
                        path.push((pos.0, row));
                    }

                    let mc = path.len() * move_cost(*c);
                    if path.iter().all(|p| s.pos.get(p) == None) {
                        let mut new_s_pos = s.pos.clone();
                        new_s_pos.remove(pos);
                        new_s_pos.insert(*pd, *c);
                        //println!("consider {} move: {:?} -> {:?}; cost={}l path={:?}", c, pos, pd, mc, path);
                        r.push((mc, State{pos: new_s_pos}));
                    } else {
                        //println!("rejected {} move: {:?} -> {:?}; cost={}; path={:?}", c, pos, pd, mc, path);
                    }
                }
            },
            v => unreachable!("Unexpected {:?} position: {:?}; state: {:?}", c, pos, v),
        }
    }

    return r;
}

fn main() {
    let m   = mk_map();
    //already solved: 0
    //let s0 = mk_state(&["ABCD", "ABCD"]);
    //slightly off: ??
    //let s0 = mk_state(&["BACD", "ABCD"]);
    //example: 12521
    //let s0 = mk_state(&["BCBD", "ADCA"]);
    let s0 = mk_state(&["BBCD", "DCAA"]);

    let mut q = BTreeSet::<(usize, State)>::new(); // (energy, State)
    let mut ans = None;

    q.insert((0, s0));

    let mut hist = BTreeMap::<State, usize>::new(); // State => energy, to skip worse repeats

    let mut steps = 0;
    loop {
        if q.is_empty() { break; }
        let (cost, s) = q.iter().next().unwrap().clone();

        if steps % 1000 == 0 { show(&m, &s, cost); } steps += 1;

        if is_final(&m, &s) {
            ans = Some(cost);
            break;
        }

        hist.insert(s.clone(), cost);

        for (extra_cost, new_state) in possible_moves(&m, &s).iter() {
            if let Some(v) = hist.get(new_state) {
                if *v <= cost + extra_cost {
                    //println!("Skip. Already considered better alt");
                    continue;
                }
            }
            q.insert((cost + extra_cost, new_state.clone()));
        }
        q.remove(&(cost, s));
    }
    println!("result: {:?}; steps={}", ans, steps);
}
