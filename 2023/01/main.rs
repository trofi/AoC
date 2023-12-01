fn solve_p1(i: &str) -> u32 {
    i.lines().map(|l| {
        let ds: Vec<u32> =
            l.chars()
             .filter(|c| c.is_digit(10))
             .map(|c| c.to_digit(10).expect("a digit"))
             .collect();
        assert!(ds.len() > 0);
        let b = ds.iter().next().expect("at least one");
        let e = ds.iter().last().expect("at least one");
        10 * b + e
    }).sum()
}

fn main() {
    let e = std::fs::read_to_string("example").expect("example");
    let i = std::fs::read_to_string("input").expect("input");
    println!("P1 example: {}", solve_p1(&e));
    println!("P1 input:   {}", solve_p1(&i));
}
