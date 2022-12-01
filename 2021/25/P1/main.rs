use std::{cmp, collections::*, fs::*, io::*, iter::*};

#[derive(Debug,Copy,Clone,Eq,PartialEq)]
enum T {
    R, // '>'
    B, // 'v'
}
use T::*;

type Pos = (usize, usize);

#[derive(Debug,Clone,Eq,PartialEq)]
struct Map {
    pos: BTreeMap<Pos, T>,
    dc: usize,
    dr: usize,
}

fn get_input(input_file: &str) -> Map {
    let reader = BufReader::new(File::open(input_file).unwrap());

    let mut pos = BTreeMap::new();
    let mut dr = 0;
    let mut dc = 0;

    for (row, ol) in reader.lines().enumerate() {
        dr = cmp::max(dr, row + 1);

        let l = ol.expect("no errors");
        for (col, c) in l.chars().enumerate() {
            dc = cmp::max(dc, col + 1);

            match c {
                '>' => { pos.insert((col, row), R); },
                'v' => { pos.insert((col, row), B); },
                '.' => {},
                _   => unreachable!("Unexpected field type {}", c),
            }
        }
    }
    return Map {
        pos,
        dc,
        dr,
    };
}

fn step_right(m: &Map) -> Map {
    let mut pos = BTreeMap::<Pos, T>::new();

    for ((c, r), v) in m.pos.iter() {
        let nc = (c + 1) % m.dc;
        let mut can_move = *v == R;
        can_move &= match m.pos.get(&(nc, *r)) {
            None    => true,
            Some(_) => false,
        };
        if can_move {
                //println!("R move {:?}: {:?} -> {:?}", v, (c, r), (nc, r));
                pos.insert((nc, *r), *v);
        } else {
                //println!("R keep {:?}: {:?}", v, (c, r));
                pos.insert((*c, *r), *v);
        }
    }

    return Map {
        pos,
        ..*m
    };
}

fn step_bottom(m: &Map) -> Map {
    let mut pos = BTreeMap::<Pos, T>::new();

    for ((c, r), v) in m.pos.iter() {
        let nr = (r + 1) % m.dr;
        let mut can_move = *v == B;
        can_move &= match m.pos.get(&(*c, nr)) {
            None    => true,
            Some(_) => false,
        };
        if can_move {
            //println!("B move {:?}: {:?} -> {:?}", v, (c, r), (c, nr));
            pos.insert((*c, nr), *v);
        } else {
            //println!("B keep {:?}: {:?}", v, (c, r));
            pos.insert((*c, *r), *v);
        }
    }

    return Map {
        pos,
        ..*m
    };
}

fn step(m: &Map) -> Map {
    let mr = step_right(m);
    return step_bottom(&mr);
}

fn pp_map(m: &Map) {
    for r in 0..m.dr {
        let mut s = String::new();
        for c in 0..m.dc {
            let v = match m.pos.get(&(c,r)) {
                None => '.',
                Some(R) => '>',
                Some(B) => 'v',
            };
            s.push(v);
        }
        println!("{}", s);
    }
}

fn main() {
    for input_file in ["example", "input"] {
        let input = get_input(input_file);
        let mut steps = 0;

        let mut m = input.clone();
        loop {
            //println!("\n-- step: {} --", steps);
            //pp_map(&input);
            let nm = step(&m);
            steps += 1;

            if m == nm { break; }

            m = nm;
        }

        pp_map(&m);
        println!("{}: {:?}", input_file, steps);
    }
}
