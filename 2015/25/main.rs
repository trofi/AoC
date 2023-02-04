fn solve(r: usize, c: usize) -> usize {
    let mut v = 20151125;
    let h = r + c - 2;
    let n = h * (h + 1) / 2 + c;

    for _ in 1..n {
        v = (v * 252533) % 33554393;
    }
    v
}

fn main() {
    println!("ans: {:?}", solve(3010, 3019));
}
