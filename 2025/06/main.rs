use std::iter::FromIterator;

#[derive(Debug)]
enum E {
    Input(&'static str, std::io::Error),
    Parse(&'static str, String),
    ParseNum(&'static str, std::num::ParseIntError),
    Eval(&'static str, char),
    Other(&'static str),
}

#[derive(Debug)]
struct Input {
    numbers: Vec<Vec<isize>>,
    ops: Vec<char>,
}

fn parse_input_p1(i: &str) -> Result<Input, E> {
    let mut numbers: Vec<Vec<isize>> = vec![];
    let mut ops: Vec<char> = vec![];

    for l in i.lines() {
        let v: Vec<&str> = l.split_whitespace().collect();

        // try numbers first
        if let Ok(nv) = v.iter().map(|e| e.parse::<isize>()).collect() {
            numbers.push(nv);
        }

        if let Ok(cv) = v.iter().map(|e|
            e.chars()
             .next()
             .ok_or_else(|| E::Parse("not a num-or-op", e.to_string()))
          ).collect() {
            ops = cv;
        }
    }

    // TODO: validate lengths

    Ok(Input{numbers, ops})
}

fn solve_p1(i: &str) -> Result<isize, E> {
    let m = parse_input_p1(i)?;
    let mut r = 0;

    for (ix, op) in m.ops.iter().enumerate() {
        let args: Vec<isize> = m.numbers.iter().map(|l| l[ix]).collect();

        r += match *op {
            '*' => args.into_iter().reduce(|a,e| a * e)
                       .ok_or_else(|| E::Eval("empty input", *op))?,
            '+' => args.into_iter().reduce(|a,e| a + e)
                       .ok_or_else(|| E::Eval("empty input", *op))?,
            _ => return Err(E::Eval("unknown op", *op)),
        };
    }

    Ok(r)
}


fn solve_p2(i: &str) -> Result<isize, E> {
    let mut lv: Vec<&str> = i.lines().collect();

    let opv: Vec<char> = lv.pop()
        .ok_or_else(|| E::Other("no op line, empty input?"))?
        .chars()
        .filter(|c| !c.is_whitespace())
        .collect();

    let mut nsv: Vec<String> = vec![];

    // vertical scan
    for ix in 0.. {
        let s = String::from_iter(lv.iter().filter_map(|l| l[ix..].chars().next()));
        if s.len() == 0 { break }
        nsv.push(s.trim().to_string());
    }

    let mut r = 0;

    for (sv, op) in nsv.split(|ns| ns == "").zip(&opv) {
        let args: Vec<isize> = sv.iter().map(|s| s.parse::<isize>())
            .collect::<Result<Vec<isize>, std::num::ParseIntError>>()
            .map_err(|e| E::ParseNum("not a number", e))?;
        r += match *op {
            '*' => args.into_iter().reduce(|a,e| a * e)
                       .ok_or_else(|| E::Eval("empty input", *op))?,
            '+' => args.into_iter().reduce(|a,e| a + e)
                       .ok_or_else(|| E::Eval("empty input", *op))?,
            _ => return Err(E::Eval("unknown op", *op)),
        };
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
