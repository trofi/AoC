fn hash(l: &str) -> usize {
    l.bytes().fold(0, |acc,v| (acc + (v as usize)) * 17 % 256)
}

fn solve_p1(i: &str) -> usize {
    i.trim().split(",").map(|l| hash(l)).sum()
}

fn solve_p2(i: &str) -> usize {
    let mut boxes: Vec<Vec<(&str, usize)>> = Vec::new();
    boxes.resize(256, vec![]);

    for ln in i.trim().split(",") {
        let lv: Vec<&str> = ln.split(&['-', '=']).collect();

        match lv.as_slice() {
            // "foo-"
            [l, ""] => {
                let bn = hash(l);
                // remove lens
                boxes[bn] = boxes[bn].iter().filter(|e| &e.0 != l).cloned().collect();
            },
            // foo=4
            [l, fs] => {
                let bn = hash(l);
                let f = fs.parse().expect("number");
                if let Some(ix) = boxes[bn].iter().position(|e| &e.0 == l) {
                    // update lens
                    boxes[bn][ix].1 = f;
                } else {
                    boxes[bn].push((l, f));
                }
            },
            _ => panic!("Unhandled {:?} operation", lv),
        }
    }

    boxes.into_iter().enumerate().map(|(bn, b)|
        (1 + bn) * b.into_iter().enumerate().map(|(ln, l)| (ln + 1) * l.1).sum::<usize>()
    ).sum()
}

fn main() {
    let e = std::fs::read_to_string("example").expect("example");
    let i = std::fs::read_to_string("input").expect("input");
    println!("P1 example: {}", solve_p1(&e));
    println!("P1   input: {}", solve_p1(&i));
    println!("P2 example: {}", solve_p2(&e));
    println!("P2   input: {}", solve_p2(&i));
}
