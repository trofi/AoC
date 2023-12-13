use std::collections::HashMap;

type Coord = (isize, isize);
type Map = HashMap<Coord, char>;

fn parse_map(i: &str) -> Map {
    let mut m = Map::new();

    for (y, l) in i.lines().enumerate() {
        for (x, c) in l.chars().enumerate() {
            m.insert((x as isize, y as isize), c);
        }
    }

    m
}

fn parse_input(i: &str) -> Vec<Map> {
    i.split("\n\n")
      .map(|l| parse_map(l))
      .collect()
}

fn solve_map(m: &Map, errs: usize) -> usize {
    let min_x = m.keys().map(|c| c.0).min().expect("one");
    let max_x = m.keys().map(|c| c.0).max().expect("one") + 1;
    let min_y = m.keys().map(|c| c.1).min().expect("one");
    let max_y = m.keys().map(|c| c.1).max().expect("one") + 1;

    // look up mirror on X axis
    for x in (min_x+1)..max_x {
        let l = std::cmp::min(max_x - x, x - min_x);
        let mut es = 0;
        for y in min_y..max_y {
            for dx in 0..l {
                if m.get(&((x - 1 - dx, y))) != m.get(&((x + dx), y)) {
                    es += 1;
                }
            }
        }

        if es == errs {
            return x as usize;
        }
    }

    // look up mirror on Y axis
    for y in (min_y+1)..max_y {
        let l = std::cmp::min(max_y - y, y - min_y);
        let mut es = 0;
        for x in min_x..max_x {
            for dy in 0..l {
                if m.get(&((x, y - 1 - dy))) != m.get(&(x, y + dy)) {
                    es += 1;
                }
            }
        }

        if es == errs {
            return 100 * y as usize;
        }
    }

    panic!("No mirrors found?");
}

fn solve_p1(i: &str) -> usize {
    let ms = parse_input(i);

    ms.into_iter().map(|m| solve_map(&m, 0)).sum()
}

fn solve_p2(i: &str) -> usize {
    let ms = parse_input(i);

    ms.into_iter().map(|m| solve_map(&m, 1)).sum()
}

fn main() {
    let e = std::fs::read_to_string("example").expect("example");
    let i = std::fs::read_to_string("input").expect("input");
    println!("P1 example: {}", solve_p1(&e));
    println!("P1   input: {}", solve_p1(&i));
    println!("P2 example: {}", solve_p2(&e));
    println!("P2   input: {}", solve_p2(&i));
}
