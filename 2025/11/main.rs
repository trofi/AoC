use std::collections::HashMap;
use std::collections::HashSet;

#[derive(Debug)]
enum E {
    Input(&'static str, std::io::Error),
    Parse(&'static str, String),
    Eval(&'static str),
}

fn parse_line(l: &str) -> Result<(&str, Vec<&str>), E> {
    let (source, targets) = l.split_once(": ")
        .ok_or_else(|| E::Parse("no ': ' separator", l.to_string()))?;

    Ok((source, targets.split(' ').collect()))
}

fn parse_input(i: &str) -> Result<Vec<(&str, Vec<&str>)>, E> {
    i.lines().map(|l| parse_line(l)).collect()
}

fn solve_from_to(from: &str, to: &str, in_edges: &HashMap<&str, HashSet<&str>>) -> Result<usize, E> {
    // ideally we should have topo-sorted it and populated values.
    // But I'm lasy and just pass through unpopulated nodes until it's
    // done.

    let mut paths: HashMap<&str, usize> = HashMap::new();
    paths.insert(from, 1);
    loop {
        let mut did_progress: bool = false;
        for (t, ss) in in_edges {
            if paths.contains_key(t) { continue }
            if ss.len() == 0 {
                paths.insert(t, 0);
                did_progress = true;
                continue
            }
            if ss.iter().any(|s| !paths.contains_key(s)) { continue }

            let cost: usize = ss.iter().map(|s| paths.get(s).expect("value")).sum();
            paths.insert(t, cost);
            did_progress = true;
        }
        if !did_progress { break }
    }

    match paths.get(to) {
       None => Err(E::Eval("no 'to' cost")),
       Some(r) => Ok(*r),
    }
}

fn solve_p1(i: &str) -> Result<usize, E> {
    let es = parse_input(i)?;

    let mut in_edges: HashMap<&str, HashSet<&str>> = HashMap::new();

    for (source, targets) in es {
        in_edges.entry(source).or_insert(HashSet::new());

        for target in &targets {
            in_edges.entry(target)
                    .and_modify(|s| { s.insert(source); })
                    .or_insert(HashSet::from([source]));
        }
    }

    solve_from_to("you", "out", &in_edges)
}

fn solve_p2(i: &str) -> Result<usize, E> {
    let es = parse_input(i)?;

    let mut in_edges: HashMap<&str, HashSet<&str>> = HashMap::new();

    for (source, targets) in es {
        in_edges.entry(source).or_insert(HashSet::new());

        for target in &targets {
            in_edges.entry(target)
                    .and_modify(|s| { s.insert(source); })
                    .or_insert(HashSet::from([source]));
        }
    }

    let svr2fft = solve_from_to("svr", "fft", &in_edges)?;
    let fft2dac = solve_from_to("fft", "dac", &in_edges)?;
    let dac2out = solve_from_to("dac", "out", &in_edges)?;

    let svr2dac = solve_from_to("svr", "dac", &in_edges)?;
    let dac2fft = solve_from_to("dac", "fft", &in_edges)?;
    let fft2out = solve_from_to("fft", "out", &in_edges)?;

    Ok(svr2fft * fft2dac * dac2out + svr2dac * dac2fft * fft2out)
}

fn main() -> Result<(), E> {
    let e = std::fs::read_to_string("example")
        .map_err(|e| E::Input("example", e))?;
    let e2 = std::fs::read_to_string("example2")
        .map_err(|e| E::Input("example2", e))?;
    let i = std::fs::read_to_string("input")
        .map_err(|e| E::Input("input", e))?;

    eprintln!("P1 e: {:?}", solve_p1(&e));
    eprintln!("P1 i: {:?}", solve_p1(&i));

    eprintln!("P2 e: {:?}", solve_p2(&e2));
    eprintln!("P2 i: {:?}", solve_p2(&i));

    Ok(())
}
