use std::collections::BTreeMap;
use std::iter::FromIterator;

type Pos = (isize, isize);
type Board = BTreeMap<Pos, char>;

fn mk_board_p1() -> Board {
    Board::from([
        ((-1, 1), '1'), ((0, 1), '2'), ((1, 1), '3'),
        ((-1, 0), '4'), ((0, 0), '5'), ((1, 0), '6'),
        ((-1,-1), '7'), ((0,-1), '8'), ((1,-1), '9'),
    ])
}

fn mk_board_p2() -> Board {
    Board::from([
                                      ((2, 2), '1'),
                       ((1, 1), '2'), ((2, 1), '3'), ((3, 1), '4'),
        ((0, 0), '5'), ((1, 0), '6'), ((2, 0), '7'), ((3, 0), '8'), ((4, 0), '9'),
                       ((1,-1), 'A'), ((2,-1), 'B'), ((3,-1), 'C'),
                                      ((2,-2), 'D'),
    ])
}

fn solve(i: &str, b: &Board) -> String {
    let mut p: Pos = (0, 0);
    let mut r: Vec<char> = Vec::new();

    for l in i.lines() {
        for c in l.chars() {
            let next_p = match c {
                'L' => (p.0 - 1, p.1),
                'R' => (p.0 + 1, p.1),
                'U' => (p.0, p.1 + 1),
                'D' => (p.0, p.1 - 1),
                _   => panic!("Unhandled {}", c),
            };
            if b.contains_key(&next_p) { p = next_p; }
        }
        r.push(*b.get(&p).expect("on square"));
    }

    String::from_iter(&r)
}

fn main() {
    let e = std::fs::read_to_string("example").expect("example");
    let i = std::fs::read_to_string("input").expect("input");
    let b1 = mk_board_p1();
    let b2 = mk_board_p2();

    println!("P1 exa: {}", solve(&e, &b1));
    println!("P1 ans: {}", solve(&i, &b1));
    println!("P2 exa: {}", solve(&e, &b2));
    println!("P2 ans: {}", solve(&i, &b2));
}
