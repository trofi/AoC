#[derive(Debug)]
enum E {
    Input(&'static str, std::io::Error),
    Parse(&'static str, char),
}

fn parse_input(i: &str) -> Result<Vec<Vec<u32>>, E> {
    i.lines().map(|l|
        l.chars().map(|c|
            c.to_digit(10).ok_or_else(|| E::Parse("not a digit", c))
        ).collect()
    ).collect()
}

fn solve(i: &str, n: usize) -> Result<usize, E> {
    let bs = parse_input(i)?;

    let mut r = 0;

    for b in &bs {
        let mut b_jol: usize = 0;
        let mut from_ix = 0;

        for n in (0..n).rev() {
            let to_ix = b.len()-n;
            let mut v = b[from_ix];
            let mut v_ix = from_ix;
            for ix in from_ix..to_ix {
                // largest digit and it's index
                if b[ix] > v {
                    v = b[ix];
                    v_ix = ix;
                }
            }

            b_jol = 10 * b_jol + (v as usize);
            from_ix = v_ix + 1;
        }

        r += b_jol;
    }

    Ok(r)
}

fn main() -> Result<(), E> {
    let e = std::fs::read_to_string("example")
        .map_err(|e| E::Input("example", e))?;
    let i = std::fs::read_to_string("input")
        .map_err(|e| E::Input("input", e))?;

    println!("P1 e: {:?}", solve(&e, 2)?);
    println!("P1 i: {:?}", solve(&i, 2)?);
    println!("P1 e: {:?}", solve(&e, 12)?);
    println!("P1 i: {:?}", solve(&i, 12)?);

    Ok(())
}
