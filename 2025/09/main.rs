use std::collections::BTreeSet;
use std::collections::HashSet;
use std::collections::VecDeque;

#[derive(Debug)]
enum E {
    Input(&'static str, std::io::Error),
    ParseNum(&'static str, std::num::ParseIntError, String),
    Parse(&'static str, String),
    Eval(&'static str),
}

type Coord = (isize, isize);

fn parse_coord(i: &str) -> Result<Coord, E> {
    let v: Vec<isize> = i.split(',')
        .map(|s| s.parse::<isize>()
                  .map_err(|e| E::ParseNum("not a number", e, s.to_string())))
        .collect::<Result<_, _>>()?;

    match v.as_slice() {
        [l, r] => Ok((*l, *r)),
        _ => Err(E::Parse("not a pair", i.to_string())),
    }
}

fn parse_input(i: &str) -> Result<Vec<Coord>, E> {
    i.lines().map(|l| parse_coord(l)).collect()
}

fn solve_p1(i: &str) -> Result<isize, E> {
    let ps = parse_input(i)?;

    let mut r = 0;

    for p1 in &ps {
        for p2 in &ps {
            let x = p1.0.max(p2.0) - p1.0.min(p2.0) + 1;
            let y = p1.1.max(p2.1) - p1.1.min(p2.1) + 1;
            r = r.max(x * y);
        }
    }

    Ok(r)
}

fn solve_p2(i: &str) -> Result<isize, E> {
    let ps = parse_input(i)?;

    // The input space is somewhat big: 100k x 100k.
    // But the point count is smaller: 250 x 250
    // Split space into segments to later do graph operation on segments.

    // BTree for sorting and unique
    let mut xs: BTreeSet<isize> = ps.iter().map(|e| e.0).collect();
    let mut ys: BTreeSet<isize> = ps.iter().map(|e| e.1).collect();

    // segments to be indexed
    let mut x_segs: Vec<(isize, isize)> = Vec::new();
    let mut y_segs: Vec<(isize, isize)> = Vec::new();

    {
        // currently tracked segment's lower bound
        let mut lx = isize::MIN;
        for x in xs {
            if lx <= x - 1 {
                x_segs.push((lx, x - 1));
                lx = x;
            }
            if lx <= x {
                x_segs.push((x, x));
                lx = x + 1;
            }
        }
        x_segs.push((lx, isize::MAX));
    }

    {
        // currently tracked segment's lower bound
        let mut ly = isize::MIN;
        for y in ys {
            if ly <= y - 1 {
                y_segs.push((ly, y - 1));
                ly = y;
            }
            if ly <= y {
                y_segs.push((y, y));
                ly = y + 1;
            }
        }
        y_segs.push((ly, isize::MAX));
    }

    type Ix = usize; // index into segment
    type ICoord = (Ix, Ix); // (x, y) point in segment space, points at [a-a] single value range.

    fn v2ix(v: isize, seg: &Vec<(isize, isize)>) -> Result<Ix, E> {
        seg.iter().position(|e| *e == (v, v)).ok_or_else(|| E::Eval("internal error, bad point"))
    }

    let mut edges: HashSet<ICoord> = HashSet::new();
    for (from, to) in ps.iter().zip(ps.iter().cycle().skip(1)) {
        let fromi = (v2ix(from.0, &x_segs)?, v2ix(from.1, &y_segs)?);
        let toi = (v2ix(to.0, &x_segs)?, v2ix(to.1, &y_segs)?);

        // horizontal
        if fromi.1 == toi.1 {
            let y = fromi.1;
            for x in fromi.0.min(toi.0)..=fromi.0.max(toi.0) {
                edges.insert((x, y));
            }
        }
        // vertical
        if fromi.0 == toi.0 {
            let x = fromi.0;
            for y in fromi.1.min(toi.1)..=fromi.1.max(toi.1) {
                edges.insert((x, y));
            }
        }
    }

    // calculate outside, bounded by one element
    let mut outside: HashSet<ICoord> = HashSet::new();
    let mut q: VecDeque<ICoord> = VecDeque::new();
    let mut visited: HashSet<ICoord> = HashSet::new();
    // start at the boundary, around (IMIN, IMIN)
    q.push_back((0, 0));
    while let Some(c) = q.pop_front() {
        if visited.contains(&c) { continue }
        visited.insert(c);
        if edges.contains(&c) { continue }
        outside.insert(c);

        for n in [(c.0.wrapping_sub(1), c.1), (c.0 + 1, c.1), (c.0, c.1.wrapping_sub(1)), (c.0, c.1 + 1)] {
            if n.0 >= x_segs.len() { continue }
            if n.1 >= y_segs.len() { continue }

            if visited.contains(&n) { continue }
            if edges.contains(&n) { continue }

            q.push_back(n);
        }
    }

    let mut r = 0;

    for p1 in &ps {
        'next_point: for p2 in &ps {
            let fx = p1.0.min(p2.0);
            let tx = p1.0.max(p2.0);
            let fy = p1.1.min(p2.1);
            let ty = p1.1.max(p2.1);

            let new_r = (tx - fx + 1) * (ty - fy + 1);
            if new_r <= r { continue }

            // check rectangle boundary for touching "outside"
            let fix = v2ix(fx, &x_segs)?;
            let tix = v2ix(tx, &x_segs)?;
            let fiy = v2ix(fy, &y_segs)?;
            let tiy = v2ix(ty, &y_segs)?;
            for xi in fix..=tix {
                if outside.contains(&(xi, fiy)) { continue 'next_point }
                if outside.contains(&(xi, tiy)) { continue 'next_point }
            }
            for yi in fiy..=tiy {
                if outside.contains(&(fix, yi)) { continue 'next_point }
                if outside.contains(&(tix, yi)) { continue 'next_point }
            }

            r = new_r;
        }
    }

    Ok(r)
}

fn main() -> Result<(), E> {
    let e = std::fs::read_to_string("example")
        .map_err(|e| E::Input("example", e))?;
    let i = std::fs::read_to_string("input")
        .map_err(|e| E::Input("input", e))?;

    println!("P1 e: {:?}", solve_p1(&e)?);
    println!("P1 i: {:?}", solve_p1(&i)?);

    println!("P2 e: {:?}", solve_p2(&e)?);
    println!("P2 i: {:?}", solve_p2(&i)?);

    Ok(())
}
