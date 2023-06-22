use std::collections::HashSet;

fn knot_hash(i: &str) -> String {
    let mut ils: Vec<usize> =
        i.bytes()
         .map(|w| w as usize)
         .collect();
    ils.extend_from_slice(&[17, 31, 73, 47, 23]);

    let mut l: Vec<usize> = (0..=255).collect();
    let mut p = 0;
    let mut ss = 0;

    for _ in 0..64 {
        for il in ils.iter().cloned() {
            let mut nl = l.clone();

            for ix in 0..il {
                nl[(p + ix) % l.len()] = l[(p + il - 1 - ix) % l.len()];
            }
            p = (p + il + ss) % l.len();
            ss += 1;

            l = nl;
        }
    }

    let mut s = String::new();
    for c in l.chunks(16) {
        let x = c.iter().cloned().reduce(|a,b| a^b).expect("at least one");
        s.push_str(&format!("{:08b}", x));
    }
    s
}

fn solve_p1(i: &str) -> usize {
    let mut used = 0;

    for rn in 0..128 {
        let r = format!("{i}-{rn}");
        let h = knot_hash(&r);

        used += h.chars().filter(|a| *a == '1').count()
    }

    used
}

fn solve_p2(i: &str) -> usize {
    type Pos = (isize, isize);
    let mut field: HashSet<Pos> = HashSet::new();

    for rn in 0..128 {
        let r = format!("{i}-{rn}");
        let h = knot_hash(&r);
        for (cn, c) in h.chars().enumerate() {
            if c == '1' {
                field.insert((cn as isize, rn as isize));
            }
        }
    }

    let mut r = 0;

    while !field.is_empty() {
        let mut q: Vec<Pos> = Vec::from([
            *field.iter().next().expect("value")
        ]);

        while let Some(p) = q.pop() {
            field.remove(&p);
            for (dc, dr) in [(-1, 0), (1, 0), (0, -1), (0, 1)] {
                let np = (p.0 + dc, p.1 + dr);
                if field.contains(&np) {
                    q.push(np);
                }
            }
        }
        r += 1;
    }

    r
}

fn main() {
    let e = "flqrgnkx";
    let i = "ljoxqyyw";
    println!("P1 example: {}", solve_p1(e));
    println!("P1 ans: {}", solve_p1(i));
    println!("P2 example: {}", solve_p2(e));
    println!("P2 ans: {}", solve_p2(i));
}
