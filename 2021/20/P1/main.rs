use std::{*, collections::*, fs::*, io::*, iter::*};

type P = (isize, isize);

#[derive(Debug,Clone)]
struct Map {
    m: Vec<bool>,
    ps: HashSet<P>,
    filler: bool,
}

fn get_input(input_file: &str) -> Map {
    let mut reader = BufReader::new(File::open(input_file).unwrap());

    let mut ns = String::new();
    reader.read_to_string(&mut ns).expect("IO error?");

    let v = Vec::from_iter(ns.split("\n\n"));

    let m = Vec::from_iter(
        v[0].chars().filter(|e| *e != '\n').map(|e|
            match e {
              '.' => false,
              '#' => true,
              _   => unreachable!("Unexpected symbol '{}' in bit map", e)
            })
    );
    let mut ps = HashSet::new();
    for (row, l) in v[1].lines().enumerate() {
        for (col, c) in l.chars().enumerate() {
            if c == '#' {
                ps.insert((col as isize, row as isize));
            }
        }
    }

    return Map{
      m: m,
      ps: ps,
      filler: false,
    };
}

fn smooth3x3(m : &Map) -> Map {
    let mut ps = HashSet::new();

    let xs = m.ps.iter().map(|p| p.0).min().unwrap();
    let xe = m.ps.iter().map(|p| p.0).max().unwrap();
    let ys = m.ps.iter().map(|p| p.1).min().unwrap();
    let ye = m.ps.iter().map(|p| p.1).max().unwrap();

    for px in xs-10..=xe+10 {
        for py in ys-10..=ye+10 {

            let mut v = 0;
            for y in [py - 1, py, py + 1] {
                for x in [px - 1, px, px + 1] {
                    v *= 2;
                    if x >= xs && x <= xe && y >= ys && y <= ye {
                        v += if m.ps.contains(&(x, y)) { 1 } else { 0 };
                    } else {
                        v += if m.filler { 1 } else { 0 };
                    }
                }
            }

            if m.m[v] {
                ps.insert((px, py));
            }
        }
    }

    return Map{
      m: m.m.clone(),
      ps: ps,
      filler: m.m[0],
    };
}

fn pp(m: &Map) {
    let xs = m.ps.iter().map(|p| p.0).min().unwrap();
    let xe = m.ps.iter().map(|p| p.0).max().unwrap();
    let ys = m.ps.iter().map(|p| p.1).min().unwrap();
    let ye = m.ps.iter().map(|p| p.1).max().unwrap();

    println!("------------------");
    for y in ys-3..=ye+3 {
        let s = String::from_iter(
          (xs-3..=xe+3).map(|x| if m.ps.contains(&(x, y)) { '#' } else { '.' })
        );
        println!("|{}|", s);
    }
    println!("------------------");
}

fn main() {
    for input_file in ["example", "input"] {
        let input = get_input(input_file);

        let mut m = input.clone();
        pp(&m);
        m = smooth3x3(&m);
        pp(&m);
        m = smooth3x3(&m);
        pp(&m);

        // 6027 is wrong
        println!("{}: {}", input_file, m.ps.len());
    }
}
