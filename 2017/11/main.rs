fn solve_p1(i: &str) -> isize {
    let (x, y): (isize, isize) =
        i.trim()
         .split(",")
         .map(|l| match l {
             "nw" => (-1, 1),
             "n"  => (0, 2),
             "ne" => (1, 1),
             "sw" => (-1, -1),
             "s"  => (0, -2),
             "se" => (1, -1),
             _ => panic!("Unknown {:?} direction", l),
         })
         .reduce(|(x,y), (dx,dy)| (x + dx, y + dy))
         .expect("At least one");
    let dx: isize = x.abs();
    let dy: isize = y.abs();
    dx + if dy > dx { (dy - dx) / 2 } else { 0 }
}

fn solve_p2(i: &str) -> isize {
    let mut r = 0;
    let mut x: isize = 0;
    let mut y: isize = 0;

    for dir in i.trim().split(",") {
        let (dx, dy) = match dir {
            "nw" => (-1, 1),
            "n"  => (0, 2),
            "ne" => (1, 1),
            "sw" => (-1, -1),
            "s"  => (0, -2),
            "se" => (1, -1),
             _ => panic!("Unknown {:?} direction", dir),
        };
        x += dx;
        y += dy;

        let ax: isize = x.abs();
        let ay: isize = y.abs();
        let nr = ax + if ay > ax { (ay - ax) / 2 } else { 0 };
        r = std::cmp::max(r, nr);
    }
    r
}


fn main() {
    let i = std::fs::read_to_string("input").expect("input");
    println!("P1 ans: {}", solve_p1(&i));
    println!("P2 ans: {}", solve_p2(&i));
}
