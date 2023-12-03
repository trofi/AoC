use std::collections::HashMap;

type Map = HashMap<(isize, isize), char>;

fn parse_map(i: &str) -> Map {
    let mut m: Map = Map::new();

    for (y, l) in i.lines().enumerate() {
        for (x, c) in l.chars().enumerate() {
            if c == '.' { continue }
            m.insert((x as isize, y as isize), c);
        }
    }

    m
}

fn solve_p1(i: &str) -> usize {
    let m = parse_map(i);

    let mut r = 0usize;

    for (y, l) in i.lines().enumerate() {
        let mut d_start = 0isize;
        let mut d_end = 0isize;
        let mut d: Option<u32> = None;

        for (x, c) in l.chars().enumerate() {
            if let Some(dc) = c.to_digit(10) {
                // aggregate a value
                d = Some(match d {
                    None    => { d_start = x as isize; d_end = x as isize; dc },
                    Some(b) => { d_end   = x as isize;                     10 * b + dc },
                });
            } else if let Some(v) = d {
                // validate value
                let mut saw_mark = false;
                for vx in (d_start - 1)..=(d_end + 1) {
                    if let Some(vc) = m.get(&(vx, y as isize - 1)) { saw_mark |= !vc.is_digit(10); }
                    if let Some(vc) = m.get(&(vx, y as isize + 1)) { saw_mark |= !vc.is_digit(10); }
                }
                if let Some(vc) = m.get(&(d_start - 1, y as isize)) { saw_mark |= !vc.is_digit(10); }
                if let Some(vc) = m.get(&(d_end   + 1, y as isize)) { saw_mark |= !vc.is_digit(10); }

                if saw_mark {
                    r += v as usize;
                }

                d = None;
                d_start = 0;
                d_end   = 0;
            }
        }
        // duplication of the above :(
        if let Some(v) = d {
            // validate value
            let mut saw_mark = false;
            for vx in (d_start - 1)..=(d_end + 1) {
                if let Some(vc) = m.get(&(vx, y as isize - 1)) { saw_mark |= !vc.is_digit(10); }
                if let Some(vc) = m.get(&(vx, y as isize + 1)) { saw_mark |= !vc.is_digit(10); }
            }
            if let Some(vc) = m.get(&(d_start - 1, y as isize)) { saw_mark |= !vc.is_digit(10); }
            if let Some(vc) = m.get(&(d_end   + 1, y as isize)) { saw_mark |= !vc.is_digit(10); }

            if saw_mark {
                r += v as usize;
            }
        }
    }

    r
}

fn solve_p2(i: &str) -> usize {
    let m = parse_map(i);

    let mut gears: HashMap<(isize, isize), Vec<usize>> = HashMap::new();

    for (y, l) in i.lines().enumerate() {
        let mut d_start = 0isize;
        let mut d_end = 0isize;
        let mut d: Option<u32> = None;

        for (x, c) in l.chars().enumerate() {
            if let Some(dc) = c.to_digit(10) {
                // aggregate a value
                d = Some(match d {
                    None    => { d_start = x as isize; d_end = x as isize; dc },
                    Some(b) => { d_end   = x as isize;                     10 * b + dc },
                });
            } else if let Some(v) = d {
                // fetch gear coords
                let mut gs: Vec<(isize, isize)> = Vec::new();

                for vx in (d_start - 1)..=(d_end + 1) {
                    if m.get(&(vx, y as isize - 1)) == Some(&'*') { gs.push((vx, y as isize - 1)); }
                    if m.get(&(vx, y as isize + 1)) == Some(&'*') { gs.push((vx, y as isize + 1)); }

                }
                if m.get(&(d_start - 1, y as isize)) == Some(&'*') { gs.push((d_start - 1, y as isize)); }
                if m.get(&(d_end   + 1, y as isize)) == Some(&'*') { gs.push((d_end   + 1, y as isize)); }

                for g in &gs {
                    gears.entry(*g)
                         .and_modify(|e| e.push(v as usize))
                         .or_insert(vec![v as usize]);
                }

                d = None;
                d_start = 0;
                d_end   = 0;
            }
        }
        // duplication of the above :(
        if let Some(v) = d {
            // fetch gear coords
            let mut gs: Vec<(isize, isize)> = Vec::new();

            for vx in (d_start - 1)..=(d_end + 1) {
                if m.get(&(vx, y as isize - 1)) == Some(&'*') { gs.push((vx, y as isize - 1)); }
                if m.get(&(vx, y as isize + 1)) == Some(&'*') { gs.push((vx, y as isize + 1)); }

            }
            if m.get(&(d_start - 1, y as isize)) == Some(&'*') { gs.push((d_start - 1, y as isize)); }
            if m.get(&(d_end   + 1, y as isize)) == Some(&'*') { gs.push((d_end   + 1, y as isize)); }

            for g in &gs {
                gears.entry(*g)
                    .and_modify(|e| e.push(v as usize))
                    .or_insert(vec![v as usize]);
            }
        }
    }

    gears.values()
         .filter(|v| v.len() == 2)
         .map(|v| v.iter().product::<usize>())
         .sum()
}

fn main() {
    let e = std::fs::read_to_string("example").expect("example");
    let i = std::fs::read_to_string("input").expect("input");
    println!("P1 example: {}", solve_p1(&e));
    println!("P1 input:   {}", solve_p1(&i));
    println!("P2 example: {}", solve_p2(&e));
    println!("P2 input:   {}", solve_p2(&i));
}
