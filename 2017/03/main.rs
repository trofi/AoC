use std::collections::BTreeMap;

fn ix_to_xy(vv: isize) -> (isize, isize) {
    if vv == 1 { return (0, 0) }
    let v = vv - 1;
    for n in 1isize.. {
        let min_n = (2 * (n - 1) + 1).pow(2);
        let max_n = (2 * n + 1).pow(2);

        if v >= max_n { continue }
        assert!(v >= min_n);

        let dv = v - min_n;
        let q  = dv / (2 * n);
        let dq = dv % (2 * n);

        let (x, y) = match q {
            0 => (          n, -n + 1 + dq),
            1 => ( n - 1 - dq,  n),
            2 => (         -n,  n - 1 - dq),
            3 => (-n + 1 + dq, -n),
            _ => panic!("Unexpected quadrant {}", q),
        };
        return (x, y);
    }
    panic!("Exhausted!")
}
fn solve_p1(vv: isize) -> isize {
    let (x, y) = ix_to_xy(vv);

    x.abs() + y.abs()
}

type Pos = (isize, isize);

fn solve_p2(n: isize) -> isize {
    let mut m: BTreeMap<Pos, isize> = BTreeMap::new();
    m.insert((0, 0), 1);

    for i in 2.. {
        let (x, y) = ix_to_xy(i);
        let mut v = 0isize;
        for dx in [-1, 0, 1] {
            for dy in [-1, 0, 1] {
                let p = (x + dx, y + dy);
                v += *m.get(&p).unwrap_or(&0);
            }
        }

        if v > n { return v }
        m.insert((x, y), v);
    }
    panic!("Did not find a value!")
}

fn main() {
    println!("P1 ans: {}", solve_p1(361527));

    println!("P2 ans: {}", solve_p2(361527));
}
