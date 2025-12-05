#[derive(Debug)]
enum E {
    Input(&'static str, std::io::Error),
    Parse(&'static str),
    ParseNum(&'static str, String, std::num::ParseIntError),
}

#[derive(Debug)]
struct I {
    ranges: Vec<(usize, usize)>,
    items: Vec<usize>,
}

fn parse_range(i: &str) -> Result<(usize, usize), E> {
    if let Some((s_l, s_r)) = i.split_once('-') {
        let l = s_l.parse::<usize>()
                    .map_err(|e| E::ParseNum("not a number", s_l.to_string(), e))?;
        let r = s_r.parse::<usize>()
                    .map_err(|e| E::ParseNum("not a number", s_r.to_string(), e))?;
        Ok((l, r))
    } else {
        Err(E::Parse("did not find '-' in range split"))
    }
}

fn parse_input(i: &str) -> Result<I, E> {
    if let Some((s_ranges, s_items)) = i.trim().split_once("\n\n") {
        let ranges: Vec<(usize, usize)> = s_ranges.lines()
            .map(|l| parse_range(l))
            .collect::<Result<Vec<_>, E>>()?;
        let items: Vec<usize> = s_items.lines()
            .map(|l| l.parse::<usize>()
                      .map_err(|e| E::ParseNum("not a number", l.to_string(), e)))
            .collect::<Result<Vec<_>, E>>()?;
        Ok(I{ranges, items})
    } else {
        Err(E::Parse("did not find double newline separator betwen ranges and items"))
    }
}

fn solve_p1(i: &str) -> Result<usize, E> {
    let i = parse_input(i)?;

    Ok(i.items.iter().filter(|e|
        i.ranges.iter().any(|r| (r.0..=r.1).contains(&e))
    ).count())
}

fn solve_p2(i: &str) -> Result<usize, E> {
    let mut ranges = parse_input(i)?.ranges;

    ranges.sort();

    let mut res = 0;
    let mut start = 0; // not included yet

    for r in &ranges {
        if r.1 < start { continue }

        let from = r.0.max(start);
        let to = r.1;

        res += to - from + 1;
        start = to + 1;
    }

    Ok(res)
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
