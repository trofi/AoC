fn ch(m: &Vec<Vec<char>>, p: (isize, isize)) -> char {
    if (m.len() as isize) <= p.0 || p.0 < 0 { return ' ' }
    if (m[p.0 as usize].len() as isize) <= p.1 || p.1 < 0 { return ' ' }

    m[p.0 as usize][p.1 as usize]
}

fn solve(i: &str) -> (String, usize) {
    let m: Vec<Vec<char>> =
        i.lines()
         .into_iter()
         .map(|l| l.chars().collect())
         .collect();

    let sy =
        m[0].iter()
            .enumerate()
            .find(|(_, v)| **v == '|')
            .expect("has entrance")
            .0 as isize;
    let mut p = (0, sy);
    let mut dir = (1, 0);
    let mut r = (String::new(), 0);

    loop {
        let c = ch(&m, p);
        match c {
            ' ' => break,
            '|' | '-' => {},
            _ if c.is_alphabetic() => { r.0.push(c) },
            '+' => {
                // (dir, U, D, L, R)
                match (dir, ch(&m, (p.0 - 1, p.1)), ch(&m, (p.0 + 1, p.1)), ch(&m, (p.0, p.1 - 1)), ch(&m, (p.0, p.1 + 1))) {
                    // U D -> L R
                    ((_, 0), _, _, ' ', _) => { dir = (0,  1); },
                    ((_, 0), _, _, _, ' ') => { dir = (0, -1); },
                    // L R -> U D
                    ((0, _), ' ', _, _, _) => { dir =  (1,  0); },
                    ((0, _), _, ' ', _, _) => { dir = (-1,  0); },
                    v => panic!("Unhandled direction {:?}", v),
                }
            },
            _ => panic!("Unexpected c={:?}", c),
        }
        p = (p.0 + dir.0, p.1 + dir.1);
        r.1 += 1;
    }

    r
}

fn main() {
    let e = std::fs::read_to_string("example").expect("example");
    let i = std::fs::read_to_string("input").expect("input");

    println!("P(1,2) example: {:?}", solve(&e));
    println!("P(1,2) ans: {:?}", solve(&i));
}
