use std::{*, convert::*, fmt::{Display,Formatter}, fs::*, io::*, str::*};

#[derive(Clone)]
enum N {
    S(usize),
    P(Box<N>, Box<N>),
}

fn pull_component(s: &[u8]) -> (N, &[u8]) {
    if let Some(d) = (s[0] as char).to_digit(10) {
        return (N::S(d.try_into().expect("signed")), &s[1..]);
    }

    assert!(s[0] == b'[');
    let (n1, san1) = pull_component(&s[1..]);
    assert!(san1[0] == b',');
    let (n2, san2) = pull_component(&san1[1..]);
    assert!(san2[0] == b']');
    return (N::P(Box::new(n1),Box::new(n2)), &san2[1..]);
}

impl FromStr for N {
    type Err = std::io::Error;

    fn from_str(s: &str) -> std::result::Result<Self, Error> {
        let (n, rest) = pull_component(s.as_bytes());
        assert!(rest.len() == 0);
        return Ok(n);
    }
}

impl Display for N {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            N::S(v) => {
                v.fmt(f)?;
            },
            N::P(n1,n2) => {
                '['.fmt(f)?;
                n1.fmt(f)?;
                ','.fmt(f)?;
                n2.fmt(f)?;
                ']'.fmt(f)?;
            }
        }
        Ok(())
    }
}

fn get_input(input_file: &str) -> Vec<N> {
    let r = BufReader::new(File::open(input_file).unwrap());

    let mut ns = Vec::new();

    for ol in r.lines() {
        let l = ol.expect("expect a line");
        ns.push(l.parse::<N>().expect("a number"));
    }

    return ns;
}

fn mag(n: &N) -> usize {
    match n {
        N::S(n) => *n,
        N::P(n1, n2) => 3 * mag(n1) +  2 * mag(n2)
    }
}

fn do_push_left(n: &mut N, dv: usize) {
    match n {
        N::S(v) => *v += dv,
        N::P(l, _) => do_push_left(l, dv),
    }
}

fn do_push_right(n: &mut N, dv: usize) {
    match n {
        N::S(v) => *v += dv,
        N::P(_, r) => do_push_right(r, dv),
    }
}


fn do_explode (n: &mut N, depth: usize) -> (bool, Option<usize>, Option<usize>) {
    match n {
        N::S(_) => return (false, None, None),
        N::P(n1, n2) => {
            match (&**n1, &**n2) {
                (N::S(v1), N::S(v2)) => {
                    if depth >= 4 {
                        let val1 : usize = *v1;
                        let val2 : usize = *v2;
                        *n = N::S(0);
                        return (true, Some(val1), Some(val2));
                    }
                    return (false, None, None);
                },
                _ => (),
            };

            match do_explode(n1, depth + 1) {
                (true, None, None) => return (true, None, None),
                (true, dl, dr) => {
                    if let Some(v) = dr {
                        do_push_left(n2, v);
                    }
                    return (true, dl, None);
                },
                (false, _, _) => {
                    match do_explode(n2, depth + 1) {
                        (true, None, None) => return (true, None, None),
                        (true, dl, dr) => {
                            if let Some(v) = dl {
                                do_push_right(n1, v);
                            }
                            return (true, None, dr);
                        },
                        (false, _, _) => (false, None, None),
                    }
                },
            }
        },
    }
}

fn explode (n: &mut N) -> bool {
    do_explode(n, 0).0
}

fn split (n: &mut N) -> bool {
    match n {
        N::S(v) => if *v > 9 {
            *n = N::P(Box::new(N::S(*v / 2)), Box::new(N::S((*v + 1) / 2)));
            return true;
        },
        N::P(n1, n2) => {
            if split (n1) { return true; }
            if split (n2) { return true; }
        }
    }
    return false;
}

fn normalize (n: &mut N) {
    loop {
        if explode(n) { continue }
        if split(n) { continue }
        break;
    }
}

fn add(n1: N, n2: N) -> N {
    let mut r = N::P(Box::new(n1), Box::new(n2));
    normalize(&mut r);
    return r;
}

fn main() {
    for input_file in ["example", "input"] {
        let input = get_input(input_file);

        let mut mags = Vec::new();

        for (i, a) in input.iter().enumerate() {
            for (j, b) in input.iter().enumerate() {
                if i != j {
                    mags.push(mag(&add(a.clone(), b.clone())));
                }
            }
        }

        println!("{}: {}", input_file,mags.iter().max().expect("at least one"));
    }
}
