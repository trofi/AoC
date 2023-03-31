fn solve_p1(i: &str) -> usize {
    let cv: Vec<usize> =
        i.trim()
         .chars()
         .map(|c| c.to_digit(10).expect("a digit") as usize)
         .collect();

    (0..cv.len()).map(|ix|
        if cv[ix] == cv[(ix + 1) % cv.len()] { cv[ix] } else { 0 }
    ).sum()
}

fn solve_p2(i: &str) -> usize {
    let cv: Vec<usize> =
        i.trim()
         .chars()
         .map(|c| c.to_digit(10).expect("a digit") as usize)
         .collect();

    (0..cv.len()).map(|ix|
        if cv[ix] == cv[(ix + cv.len() / 2) % cv.len()] { cv[ix] } else { 0 }
    ).sum()
}

fn main() {
    let i = std::fs::read_to_string("input").expect("input");
    println!("P1 ans: {}", solve_p1(&i));
    println!("P2 ans: {}", solve_p2(&i));
}
