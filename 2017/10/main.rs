fn solve_p1(n: usize, i: &str) -> usize {
    let ils: Vec<usize> =
        i.split(",")
         .map(|w| w.parse().expect("number"))
         .collect();

    let mut l: Vec<usize> = (0..=n).collect();
    let mut p = 0;

    for (ss, il) in ils.into_iter().enumerate() {
        let mut nl = l.clone();

        for ix in 0..il {
            nl[(p + ix) % l.len()] = l[(p + il - 1 - ix) % l.len()];
        }
        p = (p + il + ss) % l.len();

        l = nl;
    }

    l[0] * l[1]
}

fn solve_p2(n: usize, i: &str) -> String {
    let mut ils: Vec<usize> =
        i.bytes()
         .map(|w| w as usize)
         .collect();
    ils.extend_from_slice(&[17, 31, 73, 47, 23]);

    let mut l: Vec<usize> = (0..=n).collect();
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
        s.push_str(&format!("{:02x}", x));
    }
    s
}


fn main() {
    let i = std::fs::read_to_string("input").expect("input");
    println!("P1 example: {}", solve_p1(4, "3,4,1,5"));
    println!("P1 ans: {}", solve_p1(255, &i));

    println!("P2 empty: {}", solve_p2(255, ""));
    println!("P2 example1: {}", solve_p2(255, "AoC 2017"));
    println!("P1 example2: {}", solve_p2(255, "1,2,3"));
    println!("P1 example3: {}", solve_p2(255, "1,2,4"));
    println!("P2 ans: {}", solve_p2(255, &i));
}
