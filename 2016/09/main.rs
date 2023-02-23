use std::iter::FromIterator;

fn expand(i: &str, rec: bool) -> usize {
    let iv: Vec<char> = Vec::from_iter(i.chars());
    let mut r = 0;
    let mut ix = 0;

    while ix < iv.len() {
        let c = iv[ix];
        ix += 1;

        if c != '(' {
            r += 1;
            continue
        }

        let rix = ix +
            (&iv[ix..]).iter()
                       .position(|&v| v == ')')
                       .expect("closing bracket");

        // "lxn"
        let sv: Vec<String> =
          String::from_iter(&iv[ix..rix])
                 .split("x")
                 .map(|e| String::from(e))
                 .collect();
        let (l, n): (usize, usize) = match sv.as_slice() {
            [ls, ns] => (ls.parse().expect("l"), ns.parse().expect("n")),
            _ => panic!("Unexpected spec {:?}", sv)
        };

        r += n * if rec {
            let s: String = String::from_iter(&iv[rix+1..rix+1+l]);
            expand(&s, rec)
        } else {
            l
        };
        ix = rix + 1 + l;
    }
    r
}

fn solve(i: &str, rec: bool) -> usize {
    i.lines().map(|l| expand(l, rec)).sum()
}

fn main() {
    let e = std::fs::read_to_string("example").expect("example");
    let i = std::fs::read_to_string("input").expect("input");
    println!("P1 example: {}", solve(&e, false));
    println!("P1 ans: {}", solve(&i, false));
    println!("P2 ans: {}", solve(&i, true));
}
