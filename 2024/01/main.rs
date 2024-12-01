use std::collections::HashMap;

fn parse_input(i: &str) -> Vec<Vec<isize>> {
    i.lines().map(|l|
        l.split(" ")
         .filter(|e| *e != "")
         .map(|e| e.parse::<isize>().expect("a number"))
         .collect()
    ).collect()
}

fn solve_p1(i: &str) -> isize {
    let vi = parse_input(&i);

    let mut lv: Vec<isize> = vi.iter().map(|e| e[0]).collect();
    let mut rv: Vec<isize> = vi.iter().map(|e| e[1]).collect();
    lv.sort();
    rv.sort();

    std::iter::zip(&lv, &rv)
        .map(|(l, r)| (l - r).abs())
        .sum()
}

fn slice2hist(vs: &[isize]) -> HashMap<isize, isize> {
    let mut r = HashMap::new();

    for k in vs {
        r.entry(*k)
         .and_modify(|v| *v += 1)
         .or_insert(1);
    }

    r
}

fn solve_p2(i: &str) -> isize {
    let vi = parse_input(&i);

    let lv: Vec<isize> = vi.iter().map(|e| e[0]).collect();
    let rv: Vec<isize> = vi.iter().map(|e| e[1]).collect();

    let lh = slice2hist(&lv);
    let rh = slice2hist(&rv);

    lh.iter().map(|(k,v)|
        k * v * rh.get(k).unwrap_or(&0)
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
