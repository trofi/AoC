use std::iter::FromIterator;

fn step(i: &str) -> String {
    let vc: Vec<char> = i.chars().collect();

    String::from_iter(
        (0..i.len()).map(|ix| {
            let c3: Vec<char> = [ix - 1, ix, ix + 1].iter().map(|iy| {
                *vc.get(*iy).unwrap_or(&'.')
            }).collect();
            match c3.as_slice() {
                ['^', '^', '.'] => '^',
                ['.', '^', '^'] => '^',
                ['^', '.', '.'] => '^',
                ['.', '.', '^'] => '^',
                _               => '.',
            }
        })
    )
}

fn solve(i: &str, rows: usize) -> usize {
    let mut r = 0;
    let mut s = String::from(i);

    for _ in 0..rows {
        r += s.chars().filter(|c| *c == '.').count();
        s = step(&s);
    }
    r
}

fn main() {
    println!("One step: {} -> {}", "..^^.", step("..^^."));
    println!("P1 example: {}", solve(".^^.^.^^^^", 10));
    println!("P1 ans: {}", solve(".^^^^^.^^^..^^^^^...^.^..^^^.^^....^.^...^^^...^^^^..^...^...^^.^.^.......^..^^...^.^.^^..^^^^^...^.", 40));
    println!("P1 ans: {}", solve(".^^^^^.^^^..^^^^^...^.^..^^^.^^....^.^...^^^...^^^^..^...^...^^.^.^.......^..^^...^.^.^^..^^^^^...^.", 400000));
}
