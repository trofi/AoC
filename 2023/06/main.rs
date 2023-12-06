struct Input {
    times: Vec<isize>,
    dists: Vec<isize>,
}

fn parse_input(i: &str) -> Input {
    let iv: Vec<&str> = i.lines().collect();
    assert!(iv.len() == 2);

    let times: Vec<isize> =
        iv[0].split(&[' ', ':'])
             .filter(|e| e != &"" && e != &"Time")
             .map(|e| e.parse().expect("a number"))
             .collect();
    let dists: Vec<isize> =
        iv[1].split(&[' ', ':'])
             .filter(|e| e != &"" && e != &"Distance")
             .map(|e| e.parse().expect("a number"))
             .collect();
    Input{times, dists}
}

// largest value such that v * v <= n
fn isqrt(n: isize) -> isize {
    assert!(n as f64 != (n+1) as f64);
    let v = (n as f64).sqrt() as isize;
    let vp1 = v +1;
    assert!(v*v <= n);
    assert!(vp1*vp1 > n);
    v
}

// How many integer 'x's we have such that:
//     x * (t - x) >= d
//  or
//     -x*x + t*x - d >= 0
fn winnings(t: isize, dist: isize) -> isize {
    let d = t*t - 4 * dist;
    assert!(d >= 0);
    let mut x1 = (t - isqrt(d)) / 2 - 1;
    let mut x2 = (t + isqrt(d)) / 2 + 1;

    assert!(x1 >= 0);
    assert!(x1 <= t);
    assert!(x2 >= 0);
    assert!(x2 <= t);
    assert!(x1 * (t - x1) <= dist);
    assert!(x2 * (t - x2) <= dist);

    while x1 * (t - x1) <= dist {
        x1 += 1;
        assert!(x1 <= x2);
    }
    while x2 * (t - x2) <= dist {
        x2 -= 1;
        assert!(x1 <= x2);
    }

    x2 - x1 + 1
}

fn solve_p1(i: &str) -> isize {
    let i = parse_input(i);

    i.times.into_iter()
           .zip(i.dists.into_iter())
           .map(|(t,d)| winnings(t, d))
           .product()
}

fn main() {
    let e = std::fs::read_to_string("example").expect("example");
    let i = std::fs::read_to_string("input").expect("input");
    let e_spaceless: String = e.chars().filter(|c| *c != ' ').collect();
    let i_spaceless: String = i.chars().filter(|c| *c != ' ').collect();
    println!("P1 example: {}", solve_p1(&e));
    println!("P1   input: {}", solve_p1(&i));
    println!("P2 example: {}", solve_p1(&e_spaceless));
    println!("P2   input: {}", solve_p1(&i_spaceless));
}
