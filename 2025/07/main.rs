use std::collections::HashMap;

#[derive(Debug)]
enum E {
    Input(&'static str, std::io::Error),
    Eval(&'static str),
}

type Coord = (isize, isize);
type Map = HashMap<Coord, char>;

fn parse_input(i: &str) -> Map {
    let mut m = Map::new();

    for (y, l) in i.lines().enumerate() {
        for (x, c) in l.chars().enumerate() {
            m.insert((x as isize, y as isize), c);
        }
    }

    m
}

fn solve(i: &str) -> Result<(usize, usize), E> {
    let m = parse_input(i);

    // find start
    let s: Vec<Coord> = m.iter()
        .filter_map(|(k, v)| if *v == 'S' { Some(*k) } else { None } )
        .collect();
    let start = match s.as_slice() {
        [c] => *c,
        _ => return Err(E::Eval("did not find start")),
    };

    let mut splits = 0;

    // beam position + duplicates
    let mut b: HashMap<Coord, usize> = HashMap::new();
    b.insert(start, 1);

    loop {
        let mut next_b: HashMap<Coord, usize> = HashMap::new();

        for (c, w) in &b {
            // descend one line at a time
            let nc: Coord = (c.0, c.1 + 1);
            let ns: Vec<Coord> = match m.get(&nc) {
                Some('.') => vec![nc],
                // split
                Some('^') => { splits += 1; vec![(nc.0 - 1, nc.1), (nc.0 + 1, nc.1)] },
                Some(_) => return Err(E::Eval("Unhandled target for a beam")),
                None => vec![],
            };
            for nc in ns {
                if let Some('.') = m.get(&nc) {
                    next_b.entry(nc)
                          .and_modify(|e| { *e += *w; } )
                          .or_insert(*w);
                }
            }
        }

        if next_b.len() == 0 {
            return Ok((splits, b.values().sum()))
        }
        b = next_b;
    }
}

fn main() -> Result<(), E> {
    let e = std::fs::read_to_string("example")
        .map_err(|e| E::Input("example", e))?;
    let i = std::fs::read_to_string("input")
        .map_err(|e| E::Input("input", e))?;

    println!("P1/2 e: {:?}", solve(&e)?);
    println!("P1/2 i: {:?}", solve(&i)?);

    Ok(())
}
