use std::collections::HashSet;

fn parse_input(i: &str) -> Vec<Vec<HashSet<char>>> {
    i.trim().split("\n\n").map(|l|
        l.split("\n").map(|e|
            e.chars().collect()
        ).collect()
    ).collect()
}

fn solve_p1(i: &str) -> usize {
    let ps = parse_input(i);

    ps.into_iter().map(|g|
        g.into_iter().reduce(|acc, e|
            acc.union(&e)
               .cloned()
               .collect()
        ).expect("at least one element").len()
    ).sum()
}

fn solve_p2(i: &str) -> usize {
    let ps = parse_input(i);

    ps.into_iter().map(|g|
        g.into_iter().reduce(|acc, e|
            acc.intersection(&e)
               .cloned()
               .collect()
        ).expect("at least one element").len()
    ).sum()
}

fn main() {
    let e = std::fs::read_to_string("example").expect("example");
    let i = std::fs::read_to_string("input").expect("input");
    println!("P1 e: {:?}", solve_p1(&e));
    println!("P1 i: {:?}", solve_p1(&i));
    println!("P2 e: {:?}", solve_p2(&e));
    println!("P2 i: {:?}", solve_p2(&i));
}
