use std::collections::HashMap;

struct Rect {
    x: usize,
    y: usize,
}

struct Claim {
    id: usize,
    pos: Rect,
    size: Rect,
}

fn parse_claim(i: &str) -> Claim {
    // Example: "#1 @ 1,3: 4x4"
    let vs: Vec<usize> = i
        .split(&['#', ' ', '@', ',', ':', 'x'])
        .filter(|e| e.len() > 0)
        .map(|e| e.parse::<usize>().expect("a number"))
        .collect();
    match vs.as_slice() {
        [id, px, py, sx, sy] => Claim{
            id: *id,
            pos: Rect{x: *px, y: *py},
            size: Rect{x: *sx, y: *sy},
        },
        _ => panic!("Unsupported {:?} format", vs),
    }
}

fn parse_input(i: &str) -> Vec<Claim> {
    i.lines().map(|l| parse_claim(l)).collect()
}

fn solve_p1(i: &str) -> usize {
    let cs = parse_input(i);

    let mut claimed: HashMap<(usize, usize), usize> = HashMap::new();

    for c in &cs {
        for x in 0..c.size.x {
            for y in 0..c.size.y {
                claimed.entry((c.pos.x + x, c.pos.y + y))
                       .and_modify(|v| *v += 1)
                       .or_insert(1);
            }
        }
    }

    claimed.values().filter(|e| **e > 1).count()
}

fn is_overlapping(lx: usize, ls: usize, rx: usize, rs: usize) -> bool {
    // make sure 'l' is on the left of 'r'
    if lx > rx { return is_overlapping(rx, rs, lx, ls) }

    // check if r's start 
    return rx < lx + ls
}

fn solve_p2(i: &str) -> usize {
    let cs = parse_input(i);

    'next_id: for i in 0..cs.len() {
        for j in 0..cs.len() {
            if i == j { continue }
            if is_overlapping(cs[i].pos.x, cs[i].size.x, cs[j].pos.x, cs[j].size.x) &&
               is_overlapping(cs[i].pos.y, cs[i].size.y, cs[j].pos.y, cs[j].size.y) {
                   continue 'next_id
            }
        }
        return cs[i].id
    }
    panic!("All are overlapping!")
}

fn main() {
    let e = std::fs::read_to_string("example").expect("example");
    let i = std::fs::read_to_string("input").expect("input");
    println!("P1 e: {:?}", solve_p1(&e));
    println!("P1 i: {:?}", solve_p1(&i));
    println!("P2 e: {:?}", solve_p2(&e));
    println!("P2 i: {:?}", solve_p2(&i));
}
