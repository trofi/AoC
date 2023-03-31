fn solve_p1(i: &str) -> usize {
    i.lines().map(|l| {
        let ds: Vec<usize> =
            l.split("\t")
             .map(|e| e.parse().expect("number"))
             .collect();

        let l = ds.iter().min().expect("at least one");
        let u = ds.iter().max().expect("at least one");

        u - l
    }).sum()
}

fn solve_p2(i: &str) -> usize {
    i.lines().map(|l| {
        let ds: Vec<usize> =
            l.split("\t")
             .map(|e| e.parse().expect("number"))
             .collect();

        let mut res = None;

        for (nix, n) in ds.iter().enumerate() {
            for (dix, d) in ds.iter().enumerate() {
                if nix == dix { continue }

                if *d != 0 && *n % *d == 0 {
                    res = Some(*n / *d)
                }
            }
        }

        res.expect("found pair")
    }).sum()
}

fn main() {
    let i = std::fs::read_to_string("input").expect("input");
    println!("P1 ans: {}", solve_p1(&i));
    println!("P2 ans: {}", solve_p2(&i));
}
