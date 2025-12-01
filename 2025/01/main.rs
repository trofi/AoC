#[derive(Debug)]
enum E {
    Input(String, std::io::Result<String>),
    ParseRoll(String),
    ParseRollNum(String, std::num::ParseIntError),
}

fn parse_roll(s: &str) -> Result<isize, E> {
    let (sign, sn): (isize, &str) =  match s.split_at(1) {
        ("L", sn) => (-1, sn),
        ("R", sn) => ( 1, sn),
        _         => return Err(E::ParseRoll(s.to_string())),
    };
    let v: isize = sn.parse()
        .map_err(|e| E::ParseRollNum(s.to_string(), e))?;
    Ok(sign * v)
}

fn parse_input(i: &str) -> Result<Vec<isize>, E> {
    i.lines().map(|l| parse_roll(l)).collect()
}

fn solve_p1(i: &str) -> Result<isize, E> {
    let rs = parse_input(i)?;

    let mut s: isize = 50;
    let mut res = 0;

    for r in rs {
        s = (s + r).rem_euclid(100);
        if s == 0 { res += 1 }
    }

    Ok(res)
}

fn solve_p2(i: &str) -> Result<isize, E> {
    let rs = parse_input(i)?;

    let mut s: isize = 50;
    let mut res = 0;

    for r in rs {
        // full circles, any direction
        res += (r / 100).abs();
        let d = r % 100;

        let range = if d < 0 { (s+d)..s } else { (s+1)..(s+d+1) };

        // crosses zero or overflows slightly. Ugly.
        for z in [-100, 0, 100] {
            if range.contains(&z) { res += 1; }
        }

        s = (s + r).rem_euclid(100);
    }

    Ok(res)
}

fn main() -> Result<(), E> {
    let e = std::fs::read_to_string("example")
       .map_err(|e| E::Input("example".to_string(), Err(e)))?;
    let i = std::fs::read_to_string("input")
       .map_err(|e| E::Input("input".to_string(), Err(e)))?;

    println!("solve P1 e: {:?}", solve_p1(&e)?);
    println!("solve P1 i: {:?}", solve_p1(&i)?);
    println!("solve P2 e: {:?}", solve_p2(&e)?);
    println!("solve P2 i: {:?}", solve_p2(&i)?);

    Ok(())
}
