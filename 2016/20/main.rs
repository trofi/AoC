fn solve_p1(i: &str) -> usize {
    let mut vb: Vec<bool> = Vec::new();

    vb.resize(u32::MAX as usize + 1, false);

    for l in i.lines() {
        let vp: Vec<usize> =
            l.split("-")
             .map(|e| e.parse().expect("number"))
             .collect();
        assert!(vp.len() == 2);
        for ix in vp[0]..=vp[1] {
            vb[ix] = true;
        }
    }
    for (ix, v) in vb.into_iter().enumerate() {
        if !v { return ix }
    }
    panic!("No solutions!")
}

fn solve_p2(i: &str) -> usize {
    let mut vb: Vec<bool> = Vec::new();

    vb.resize(u32::MAX as usize + 1, false);

    for l in i.lines() {
        let vp: Vec<usize> =
            l.split("-")
             .map(|e| e.parse().expect("number"))
             .collect();
        assert!(vp.len() == 2);
        for ix in vp[0]..=vp[1] {
            vb[ix] = true;
        }
    }
    let mut r = 0;
    for (ix, v) in vb.into_iter().enumerate() {
        if !v { r += 1 }
    }
    r
}

fn main() {
    let i = std::fs::read_to_string("input").expect("input");
    println!("P1 ans: {}", solve_p1(&i));
    println!("P2 ans: {}", solve_p2(&i));
}
