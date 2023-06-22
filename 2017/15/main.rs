const G1F: u64 = 16807;
const G2F: u64 = 48271;

const M: u64 = 2147483647;

fn solve_p1((g1s, g2s): (u64, u64)) -> usize {
    let mut r = 0;

    let mut g1 = g1s;
    let mut g2 = g2s;
    for _ in 0..40_000_000 {
        g1 = g1 * G1F % M;
        g2 = g2 * G2F % M;

        if g1 & 0xFFff == g2 & 0xFFff {
            r += 1;
        }
    }

    r
}

fn solve_p2((g1s, g2s): (u64, u64)) -> usize {
    let mut r = 0;

    let mut g1 = g1s;
    let mut g2 = g2s;
    for _ in 0..5_000_000 {
        loop {
            g1 = g1 * G1F % M;
            if g1 % 4 == 0 { break }
        }
        loop {
            g2 = g2 * G2F % M;
            if g2 % 8 == 0 { break }
        }

        if g1 & 0xFFff == g2 & 0xFFff {
            r += 1;
        }
    }

    r
}


fn main() {
    let e = (65, 8921);
    let i = (512, 191);

    println!("P1 example: {}", solve_p1(e));
    println!("P1 ans: {}", solve_p1(i));
    println!("P2 example: {}", solve_p2(e));
    println!("P2 ans: {}", solve_p2(i));
}
