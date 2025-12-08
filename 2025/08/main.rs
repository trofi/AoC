use std::collections::HashSet;
use std::iter::FromIterator;

#[derive(Debug)]
enum E {
    Input(&'static str, std::io::Error),
    Parse(&'static str, String),
    Eval(&'static str),
}

type Coord = (isize, isize, isize);

fn parse_box(l: &str) -> Result<Coord, E> {
    let nv: Vec<isize> = l.split(',')
        .map(|e| e.parse::<isize>().map_err(|e| E::Parse("num parse error", l.to_string())))
        .collect::<Result<_, E>>()?;

    match nv.as_slice() {
        &[a,b,c] => Ok((a,b,c)),
        _       => Err(E::Parse("incorrect element count", l.to_string())),
    }
}

fn parse_input(i: &str) -> Result<Vec<Coord>, E> {
    i.lines().map(|l| parse_box(l)).collect()
}

fn solve_p1(i: &str, topn: usize) -> Result<usize, E> {
    let ns = parse_input(i)?;
    // node index, we'll do graps operations on indices.
    type ix = usize;

    let mut dists: Vec<(isize, (ix, ix))> = Vec::new();
    for i in 0..ns.len() {
        for j in (i+1)..ns.len() {
            let dist = (ns[i].0 - ns[j].0).pow(2)
                     + (ns[i].1 - ns[j].1).pow(2)
                     + (ns[i].2 - ns[j].2).pow(2);
            dists.push((dist, (i, j)));
        }
    }
    dists.sort();

    // find SCCs: one SCC is a set of indices

    // initialize SCCs: each is a singleton
    let mut sccs: Vec<HashSet<ix>> = Vec::new();
    // map from node index to a corresponding scc
    let mut ix2scc: Vec<ix> = Vec::new();
    for i in 0..ns.len() {
        let mut scc: HashSet<ix> = HashSet::new();
        scc.insert(i);
        sccs.push(scc);
        ix2scc.push(i);
    }

    // construct SCCs using adjacency data
    for (_, (i, j)) in dists.into_iter().take(topn) {
        let mut iscc = ix2scc[i];
        let mut jscc = ix2scc[j];
        if iscc == jscc { continue }

        // make iscc a larger one
        if sccs[iscc].len() < sccs[jscc].len() {
            let t = jscc;
            jscc = iscc;
            iscc = t;
        }

        // merge smaller scc into larger
        // TODO: can we merge sets in a vector without a copy?
        let es: HashSet<ix> = HashSet::from_iter(sccs[jscc].drain());
        for e in es.into_iter() {
            sccs[iscc].insert(e);
            ix2scc[e] = iscc;
        }
    }

    let mut scc_sizes: Vec<usize> = sccs
        .into_iter()
        .map(|s| s.len())
        .collect();
    scc_sizes.sort();

    Ok(scc_sizes.into_iter().rev().take(3).product())
}

fn solve_p2(i: &str) -> Result<isize, E> {
    let ns = parse_input(i)?;
    // node index, we'll do graps operations on indices.
    type ix = usize;

    let mut dists: Vec<(isize, (ix, ix))> = Vec::new();
    for i in 0..ns.len() {
        for j in (i+1)..ns.len() {
            let dist = (ns[i].0 - ns[j].0).pow(2)
                     + (ns[i].1 - ns[j].1).pow(2)
                     + (ns[i].2 - ns[j].2).pow(2);
            dists.push((dist, (i, j)));
        }
    }
    dists.sort();

    // find SCCs: one SCC is a set of indices
    // initialize SCCs: each is a singleton
    let mut sccs: Vec<HashSet<ix>> = Vec::new();
    // map from node index to a corresponding scc
    let mut ix2scc: Vec<ix> = Vec::new();
    for i in 0..ns.len() {
        let mut scc: HashSet<ix> = HashSet::new();
        scc.insert(i);
        sccs.push(scc);
        ix2scc.push(i);
    }

    // construct SCCs using adjacency data
    for (_, (i, j)) in dists.into_iter() {
        let mut iscc = ix2scc[i];
        let mut jscc = ix2scc[j];
        if iscc == jscc { continue }

        // make iscc a larger one
        if sccs[iscc].len() < sccs[jscc].len() {
            let t = jscc;
            jscc = iscc;
            iscc = t;
        }

        // merge smaller scc into larger
        // TODO: can we merge sets in a vector without a copy?
        let es: HashSet<ix> = HashSet::from_iter(sccs[jscc].drain());
        for e in es.into_iter() {
            sccs[iscc].insert(e);
            ix2scc[e] = iscc;
        }

        // collected them all!
        if sccs[iscc].len() == ns.len() {
            return Ok(ns[i].0 * ns[j].0)
        }
    }

    Err(E::Eval("no solution"))
}

fn main() -> Result<(), E> {
    let e = std::fs::read_to_string("example")
        .map_err(|e| E::Input("example", e))?;
    let i = std::fs::read_to_string("input")
        .map_err(|e| E::Input("input", e))?;

    println!("P1 e: {:?}", solve_p1(&e, 10)?);
    println!("P1 i: {:?}", solve_p1(&i, 1000)?);

    println!("P2 e: {:?}", solve_p2(&e)?);
    println!("P2 i: {:?}", solve_p2(&i)?);

    Ok(())
}
