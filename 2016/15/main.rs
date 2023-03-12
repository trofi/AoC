#[derive(Debug)]
struct Disc {
    n: usize,
    m: usize,
    p: usize,
}

fn solve(i: &str, is_p2: bool) -> usize {
    let mut ds: Vec<Disc> =
        i.lines()
         .map(|l| {
             let vs: Vec<_> = l.split([' ', '#', '.']).collect();
             match vs.as_slice() {
                 ["Disc", "", sn,
                  "has", sm, "positions;", "at", "time=0,",
                  "it", "is", "at", "position", sp, ""] =>
                      Disc{
                          n: sn.parse().expect("disc number"),
                          m: sm.parse().expect("disc modulo"),
                          p: sp.parse().expect("disc position"),
                      },
                 _ => panic!("Unhandled {:?}", vs)
             }
         })
         .collect();

    if is_p2 {
        ds.push(Disc{
            n: ds.len() + 1,
            m: 11,
            p: 0,
        });
    }

    't: for t in 0.. {
        for d in &ds {
            if (t + d.p + d.n) % d.m != 0 { continue 't }
        }
        return t
    }

    panic!("No solution!")
}

fn main() {
    let e = std::fs::read_to_string("example").expect("example");
    let i = std::fs::read_to_string("input").expect("input");

    println!("P1 example: {:?}", solve(&e, false));
    println!("P1 ans: {:?}", solve(&i, false));
    println!("P2 ans: {:?}", solve(&i, true));
}
