use std::collections::HashSet;

type Coord = (isize, isize);
type Map = HashSet<Coord>;

fn parse_input(i: &str) -> Vec<Map> {
    i.split("\n\n")
     .map(|e| {
         let mut m = Map::new();

         for (y, l) in e.lines().enumerate() {
             for (x, c) in l.chars().enumerate() {
                 if c == '#' {
                     m.insert((x as isize, y as isize));
                 }
             }
         }

         m
     })
    .collect()
}

fn solve_p1(i: &str) -> usize {
    let mv = parse_input(&i);

    let mut r = 0;

    for lix in 0..mv.len() {
        let mls = &mv[lix];

        for rix in lix+1..mv.len() {
            let mrs = &mv[rix];

            if mls.intersection(&mrs).count() == 0 {
                r += 1;
            }
        }
    }

    r
}

fn main() {
    let e = std::fs::read_to_string("example").expect("example");
    let i = std::fs::read_to_string("input").expect("input");
    println!("P1 e: {:?} (expect 3)", solve_p1(&e));
    println!("P1 i: {:?}", solve_p1(&i));
}
