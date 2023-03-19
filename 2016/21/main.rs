use std::iter::FromIterator;

#[derive(Debug, Clone, Copy)]
enum Cmd {
    SwapPos(usize, usize),
    SwapLetter(char, char),
    ReversePos(usize, usize),
    RotateLeft(usize),
    RotateRight(usize),
    MovePos(usize, usize),
    RotateLetter(char),
}

fn parse_prog(i: &str) -> Vec<Cmd> {
    i.lines().map(|l|{
        let v: Vec<_> = l.split(' ').collect();
        match v.as_slice() {
            ["swap", "position", ps1, "with", "position", ps2] =>
                Cmd::SwapPos(
                    ps1.parse().expect("number"),
                    ps2.parse().expect("number")),
            ["swap", "letter", ls1, "with", "letter", ls2] =>
                Cmd::SwapLetter(
                    ls1.parse().expect("char"),
                    ls2.parse().expect("char")),
            ["reverse", "positions", ps1, "through", ps2] =>
                Cmd::ReversePos(
                    ps1.parse().expect("number"),
                    ps2.parse().expect("number")),
            ["rotate", "left", ss, "step"] =>
                Cmd::RotateLeft(ss.parse().expect("number")),
            ["rotate", "left", ss, "steps"] =>
                Cmd::RotateLeft(ss.parse().expect("number")),
            ["rotate", "right", ss, "step"] =>
                Cmd::RotateRight(ss.parse().expect("number")),
            ["rotate", "right", ss, "steps"] =>
                Cmd::RotateRight(ss.parse().expect("number")),
            ["move", "position", ps1, "to", "position", ps2] =>
                Cmd::MovePos(
                    ps1.parse().expect("number"),
                    ps2.parse().expect("number")),
            ["rotate", "based", "on", "position", "of", "letter", ls] =>
                Cmd::RotateLetter(
                    ls.parse().expect("char")),
            _ => panic!("Unupported {:?}", v)
        }
    }).collect()
}

fn run(prog: &[Cmd], key: &str) -> String {
    let mut vk: Vec<char> = key.chars().collect();

    for cmd in prog {
        let mut vc: Vec<char>;
        match *cmd {
            Cmd::SwapPos(f, t) => {
                vc = vk.clone();
                vc[f] = vk[t];
                vc[t] = vk[f];
            },
            Cmd::SwapLetter(l1, l2) => {
                vc = vk.iter().map(|c|
                  if      *c == l1 { l2 }
                  else if *c == l2 { l1 }
                  else             { *c }
                ).collect();
            },
            Cmd::ReversePos(f, t) => {
                vc = vk.clone();
                for ix in f..=t {
                    vc[ix] = vk[t + f - ix];
                }
            },
            Cmd::RotateLeft(dix) => {
                vc = (0..vk.len()).map(|ix|
                    vk[(ix + dix) % vk.len()]
                ).collect()
            },
            Cmd::RotateRight(dix) => {
                vc = (0..vk.len()).map(|ix|
                    vk[(ix + vk.len() - dix) % vk.len()]
                ).collect()
            },
            Cmd::MovePos(f, t) => {
                vc = vk.clone();
                let v = vc.remove(f);
                vc.insert(t, v);
            },
            Cmd::RotateLetter(l) => {
                let dix =
                    vk.iter()
                      .enumerate()
                      .find(|(_, v)| l == **v)
                      .expect("ix").0;
                let roff = 1 + dix + if dix >= 4 { 1 } else { 0 };
                vc = (0..vk.len()).map(|ix|
                    vk[(ix + 2 * vk.len() - roff) % vk.len()]
                ).collect()
            }
        }
        vk = vc;
    }

    String::from_iter(&vk)
}

fn validate(cmd: &Cmd, before: &[char], after: &[char]) {
    let bv = String::from_iter(before);
    let av = String::from_iter(after);
    let r = run(&[*cmd], &bv);
    if av != r {
        panic!("Failed to revert cmd={:?} for av={:?}; got r={:?} for bv={:?}", cmd, av, r, bv);
    }
}

fn unrun(prog: &[Cmd], key: &str) -> String {
    let mut vk: Vec<char> = key.chars().collect();

    for cmd in prog.iter().rev() {
        let mut vc: Vec<char>;
        match *cmd {
            Cmd::SwapPos(f, t) => {
                vc = vk.clone();
                vc[f] = vk[t];
                vc[t] = vk[f];
            },
            Cmd::SwapLetter(l1, l2) => {
                vc = vk.iter().map(|c|
                                   if      *c == l1 { l2 }
                                   else if *c == l2 { l1 }
                                   else             { *c }
                                  ).collect();
            },
            Cmd::ReversePos(f, t) => {
                vc = vk.clone();
                for ix in f..=t {
                    vc[ix] = vk[t + f - ix];
                }
            },

            // right<->left are flipped
            Cmd::RotateRight(dix) => {
                vc = (0..vk.len()).map(|ix|
                                       vk[(ix + dix) % vk.len()]
                                      ).collect();
            },
            Cmd::RotateLeft(dix) => {
                vc = (0..vk.len()).map(|ix|
                                       vk[(ix + vk.len() - dix) % vk.len()]
                                      ).collect();
            },

            Cmd::MovePos(f, t) => {
                vc = vk.clone();
                let v = vc.remove(t);
                vc.insert(f, v);
            },
            // - invert '1 + ix + 4 + {0,1}'
            // - shift left
            Cmd::RotateLetter(l) => {
                assert!(vk.iter().filter(|e| **e == l).count() == 1);
                let dix =
                    vk.iter()
                    .enumerate()
                    .find(|(_, v)| l == **v)
                    .expect("ix").0;
                // Generated with:
                //   for dix in 0..8 { let o = (1 + dix + if dix >= 4 { 1 } else { 0 }) % 8; let k = (dix + o) % 8;  println!("{k} => {o},"); }
                let loff: usize = match dix {
                    0 => 1,
                    1 => 1,
                    2 => 6,
                    3 => 2,
                    4 => 7,
                    5 => 3,
                    6 => 0,
                    7 => 4,
                    _ => panic!("Unhandled {}", dix),
                };
                vc =
                    (0..vk.len())
                    .map(|ix|
                         vk[(ix + loff) % vk.len()]
                        ).collect();
            }
        }
        validate(cmd, &vc, &vk);
            vk = vc;
    }

    String::from_iter(&vk)
}

fn solve(i: &str, key: &str) -> String {
    let p = parse_prog(i);

    run(&p, key)
}

fn solve_p2(i: &str, key: &str) -> String {
    let p = parse_prog(i);

    let r = unrun(&p, key);

    assert!(key == run(&p, &r));

    r
}

fn main() {
    let e = std::fs::read_to_string("example").expect("example");
    let i = std::fs::read_to_string("input").expect("input");
    println!("P1 example: {}", solve(&e, "abcde"));
    println!("P1 ans: {}", solve(&i, "abcdefgh"));
    println!("P2 ans: {}", solve_p2(&i, "fbgdceah"));
}
