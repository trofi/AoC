type Pos = (isize, isize);

#[derive(Debug)]
struct Desc {
    a: Pos,
    b: Pos,
    p: Pos,
}

fn parse_desc(i: &str) -> Desc {
    let vi: Vec<&str> = i
        .split(&[' ', ':', ',', '\n', '='])
        .filter(|l| *l != "")
        .collect();

    match vi.as_slice() {
        ["Button", "A", axs, ays, "Button", "B", bxs, bys, "Prize", "X", pxs, "Y", pys] =>
            Desc{
                a: (axs[1..].parse::<isize>().expect("ax number"),
                    ays[1..].parse::<isize>().expect("ay number")),
                b: (bxs[1..].parse::<isize>().expect("bx number"),
                    bys[1..].parse::<isize>().expect("by number")),
                p: (pxs.parse::<isize>().expect("px number"),
                    pys.parse::<isize>().expect("py number")),
            },
        _ => panic!("Unhandled {:?} input", vi),
    }
}

fn parse_input(i: &str) -> Vec<Desc> {
    i.split("\n\n").map(|l| parse_desc(l.trim())).collect()
}

fn solve_one(d: &Desc) -> isize {
    // Don't have to handle zeros. Yay :)
    assert!(d.a.0 > 0);
    assert!(d.a.1 > 0);
    assert!(d.b.0 > 0);
    assert!(d.b.1 > 0);
    assert!(d.p.0 > 0);
    assert!(d.p.1 > 0);

    // We are MIN-imising th value of:
    //     3 * ta + tb
    // for a system of equations:
    //     ta * A.X + tb * B.X = P.X
    //     ta * A.Y + tb * B.Y = P.Y
    // and inequalities:
    //     ta >= 0
    //     tb >= 0

    // For all non-zero A.X/A.Y/B.X/B.Y those should be:
    //
    //     ta = (P.X * B.Y - P.Y * B.X) / (A.X * B.Y - A.Y * B.X)
    //     tb = (P.Y * A.X - P.X * A.Y) / (A.X * B.Y - A.Y * B.X)
    //
    // as long ad the denominator is not zero.

    let tan = d.p.0 * d.b.1 - d.p.1 * d.b.0;
    let tbn = d.p.1 * d.a.0 - d.p.0 * d.a.1;
    let den = d.a.0 * d.b.1 - d.a.1 * d.b.0;
    assert!(den != 0);

    if tan % den == 0 && tbn % den == 0 {
        (tan / den) * 3 + tbn / den
    } else {
        0
    }
}

fn solve_p1(i: &str) -> isize {
    let descs = parse_input(i);

    descs.into_iter().map(|d| solve_one(&d)).sum()
}

fn solve_p2(i: &str) -> isize {
    let mut descs: Vec<Desc> = parse_input(i);
    for e in descs.iter_mut() {
        e.p.0 += 10000000000000;
        e.p.1 += 10000000000000;
    }

    descs.into_iter().map(|d| solve_one(&d)).sum()
}

fn main() {
    let e = std::fs::read_to_string("example").expect("example");
    let i = std::fs::read_to_string("input").expect("input");
    println!("P1 e: {:?}", solve_p1(&e));
    println!("P1 i: {:?}", solve_p1(&i));
    println!("P2 e: {:?}", solve_p2(&e));
    println!("P2 i: {:?}", solve_p2(&i));
}
