use std::collections::HashSet;
use std::cmp::{min,max};

struct Cmd<'a> {
    dir: &'a str,
    len: isize,
    color: &'a str,
}

type Coord = (isize, isize);

fn parse_input(i: &str) -> Vec<Cmd> {
    i.lines().map(|l| {
        let v: Vec<&str> = l.split(&[' ', '(', ')']).collect();
        match v.as_slice() {
            [dir, slen, "", color, ""] => Cmd {
                dir,
                len: slen.parse().expect("a number"),
                color,
            },
            _ => panic!("Unhandled {:?}", v),
        }
    }).collect()
}

fn parse_color(i: &str) -> Cmd {
    // expected input: #abc123
    assert!(i.len() == 7);
    let dir = match &i[6..7] {
        "0" => "R",
        "1" => "D",
        "2" => "L",
        "3" => "U",
        _ => panic!("Unhandled direction {:?}", &i[5..6]),
    };
    let len = isize::from_str_radix(&i[1..6], 16).expect("a hex");

    Cmd {
        dir,
        len,
        color: "",
    }
}

fn solve_p1(i: &str) -> isize {
    let cmds = parse_input(i);

    let mut outline: HashSet<Coord> = HashSet::new();
    let mut pos: Coord = (0, 0);
    outline.insert(pos);

    // trace the outline
    for cmd in &cmds {
        let d = match cmd.dir {
            "R" => ( 1, 0),
            "L" => (-1, 0),
            "U" => ( 0, 1),
            "D" => ( 0,-1),
            _ => panic!("UNhandled {:?} direction", cmd.dir),
        };
        for _ in 0..cmd.len {
            pos = (pos.0 + d.0, pos.1 + d.1);
            outline.insert(pos);
        }
    }

    // fill the exterior around outline
    let min_x: isize = outline.iter().map(|c| c.0).min().expect("one") - 1;
    let max_x: isize = outline.iter().map(|c| c.0).max().expect("one") + 1;
    let min_y: isize = outline.iter().map(|c| c.1).min().expect("one") - 1;
    let max_y: isize = outline.iter().map(|c| c.1).max().expect("one") + 1;

    let mut q: Vec<Coord> = Vec::new();
    let mut visited: HashSet<Coord> = HashSet::new();

    q.push((min_x, min_y));

    while let Some(c) = q.pop() {
        if visited.contains(&c) { continue }
        if c.0 < min_x || c.0 > max_x { continue }
        if c.1 < min_y || c.1 > max_y { continue }
        if outline.contains(&c) { continue }

        visited.insert(c);
        for d in [(-1, 0), (1, 0), (0, -1), (0, 1)] {
            let nc = (c.0 + d.0, c.1 + d.1);
            q.push(nc);
        }
    }

    (max_x - min_x + 1) * (max_y - min_y + 1) - (visited.len() as isize)
}

fn solve_p2(i: &str) -> isize {
    let cmds: Vec<Cmd> = parse_input(i)
        .into_iter()
        .map(|cmd| parse_color(cmd.color))
        .collect();

    // find all the trenches to later build the intervals
    let mut trenches_x: HashSet<isize> = HashSet::new();
    let mut trenches_y: HashSet<isize> = HashSet::new();

    let mut pos: Coord = (0, 0);
    trenches_x.insert(pos.0);
    trenches_y.insert(pos.1);

    // trace the outline to get axis values
    for cmd in &cmds {
        let d = match cmd.dir {
            "R" => ( 1, 0),
            "L" => (-1, 0),
            "U" => ( 0, 1),
            "D" => ( 0,-1),
            _ => panic!("Unhandled {:?} direction", cmd.dir),
        };
        pos = (pos.0 + d.0 * cmd.len, pos.1 + d.1 * cmd.len);

        trenches_x.insert(pos.0);
        trenches_y.insert(pos.1);
    }

    // sorted trenches
    let stx: Vec<isize> = {
        let mut v: Vec<isize> = trenches_x.into_iter().collect();
        v.sort();
        v
    };
    let sty: Vec<isize> = {
        let mut v: Vec<isize> = trenches_y.into_iter().collect();
        v.sort();
        v
    };

    // Build intervals out of trenches.
    fn build_intervals(ts: &[isize]) -> Vec<(isize, isize)> {
        assert!(ts.len() > 0);

        let mut r: Vec<(isize, isize)> = Vec::new();

        // Make sure we have an inteval before the first trench
        let f: isize = *ts.first().expect("one");
        let mut lt = f - 1;

        for t in ts {
            // built at most 2 intervals:
            // [lt .. t-1] [t .. t]
            // and prepare for [t+1 .. ] interval
            assert!(lt <= t - 1);
            r.push((lt, t - 1));
            r.push((*t, *t));

            lt = *t + 1;
        }

        // ... and after the last trench to make next filling simpler.
        r.push((lt, lt));

        r
    }

    let ixs: Vec<(isize, isize)> = build_intervals(&stx);
    let iys: Vec<(isize, isize)> = build_intervals(&sty);

    // position in terms of interval number
    type ICoord = (usize, usize);

    fn p2s(pos: &Coord, ixs: &Vec<(isize, isize)>, iys: &Vec<(isize, isize)>) -> ICoord {
        let x = ixs.iter().position(|e| *e == (pos.0, pos.0)).expect("position x");
        let y = iys.iter().position(|e| *e == (pos.1, pos.1)).expect("position y");

        (x, y)
    }

    // Now we can translate the problem into original form by mapping
    // each interval into a single "square"

    let mut outline: HashSet<ICoord> = HashSet::new();
    let mut pos: Coord = (0, 0);
    let mut ipos: ICoord = p2s(&pos, &ixs, &iys);
    outline.insert(ipos);

    // trace the outline, mark visited intervals within trenches
    for cmd in &cmds {
        let d = match cmd.dir {
            "R" => ( 1, 0),
            "L" => (-1, 0),
            "U" => ( 0, 1),
            "D" => ( 0,-1),
            _ => panic!("UNhandled {:?} direction", cmd.dir),
        };
        let npos = (pos.0 + d.0 * cmd.len, pos.1 + d.1 * cmd.len);
        let nipos = p2s(&npos, &ixs, &iys);

        for x in min(ipos.0, nipos.0)..=max(ipos.0, nipos.0) {
            for y in min(ipos.1, nipos.1)..=max(ipos.1, nipos.1) {
                outline.insert((x, y));
            }
        }

        pos = npos;
        ipos = nipos;
    }

    // fill the exterior around outline
    let mut q: Vec<ICoord> = Vec::new();
    let mut visited: HashSet<ICoord> = HashSet::new();

    q.push((0, 0));

    while let Some(c) = q.pop() {
        if visited.contains(&c) { continue }
        if outline.contains(&c) { continue }
        if c.0 >= ixs.len() { continue }
        if c.1 >= iys.len() { continue }

        visited.insert(c);
        if c.0 > 0 { q.push((c.0 - 1, c.1)); }
        if c.1 > 0 { q.push((c.0, c.1 - 1)); }
        q.push((c.0 + 1, c.1));
        q.push((c.0, c.1 + 1));
    }

    // calculate an unvisited area
    let mut r: isize = 0;

    for x in 0..ixs.len() {
        for y in 0..iys.len() {
            if visited.contains(&(x, y)) { continue }

            let ix = ixs[x];
            let iy = iys[y];

            r += (ix.1 - ix.0 + 1) * (iy.1 - iy.0 + 1);
        }
    }

    r
}

fn main() {
    let e = std::fs::read_to_string("example").expect("example");
    let i = std::fs::read_to_string("input").expect("input");
    println!("P1 example: {}", solve_p1(&e));
    println!("P1   input: {}", solve_p1(&i));
    println!("P2 example: {}", solve_p2(&e));
    println!("P2   input: {}", solve_p2(&i));
}
