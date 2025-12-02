use std::collections::HashSet;
use std::iter::FromIterator;

#[derive(Debug)]
enum E {
    Input(std::io::Error, &'static str),
    ParseRange(&'static str, String),
    ParseNumber(&'static str, std::num::ParseIntError, String, String),
}

fn parse_range(l: &str) -> Result<(isize, isize), E> {
    match l.split_once('-') {
        Some((ln, rn)) => {
            let l = ln.parse::<isize>()
                .map_err(|e| E::ParseNumber("failed to parse left value in range", e, ln.to_string(), l.to_string()))?;
            let r = rn.parse::<isize>()
                .map_err(|e| E::ParseNumber("failed to parse right value in range", e, rn.to_string(), l.to_string()))?;
            Ok((l, r))
        },
        None => Err(E::ParseRange("'-' separator is missing", l.to_string())),
    }
}

fn parse_input(i: &str) -> Result<Vec<(isize, isize)>, E> {
    i.trim().split(',').map(|l| parse_range(l)).collect()
}

fn solve_p1(i: &str) -> Result<isize, E> {
    let rs = parse_input(i)?;

    let mut res = 0;

    for r in &rs {
        for n in r.0..=r.1 {
            let sn = format!("{}", n);
            let (u, b) = sn.split_at(sn.len() / 2);
            if u == b {
                res += n;
            }
        }
    }

    Ok(res)
}

fn solve_p2(i: &str) -> Result<isize, E> {
    let rs = parse_input(i)?;

    let mut res = 0;

    for r in &rs {
        'next_number: for n in r.0..=r.1 {
            let sn = format!("{}", n);
            for cl in 1..sn.len() {
                let unique_pieces = HashSet::<&[char]>::from_iter(sn.chars().collect::<Vec<char>>().chunks(cl)).len();
                if unique_pieces == 1 {
                    res += n;
                    continue 'next_number
                }
            }
        }
    }

    Ok(res)
}

fn main() -> Result<(), E> {
    let e = std::fs::read_to_string("example")
        .map_err(|e| E::Input(e, "example"))?;
    let i = std::fs::read_to_string("input")
        .map_err(|e| E::Input(e, "input"))?;

    println!("solpve P1 e: {:?}", solve_p1(&e));
    println!("solpve P1 i: {:?}", solve_p1(&i));

    println!("solpve P2 e: {:?}", solve_p2(&e));
    println!("solpve P2 i: {:?}", solve_p2(&i));

    Ok(())
}
