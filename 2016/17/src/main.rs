use std::collections::BinaryHeap;

fn dirs(key: &str, path: &str) -> Vec<bool> {
    let d = md5::compute(format!("{key}{path}"));
    let s = format!("{d:x}");
    s.chars()
     .take(4)
     .map(|c| c >= 'b')
     .collect()
}

#[derive(Debug, Eq, PartialEq, Ord, PartialOrd)]
struct Key {
    path: String,
    pos: (isize, isize),
}

fn with_prio(k: Key) -> (isize, Key) {
    let dp = (3, 3);
    let xd = (dp.0 - k.pos.0).abs();
    let yd = (dp.1 - k.pos.1).abs();
    (- (xd + yd + k.path.len() as isize), k)
}

fn solve_p1(key: &str) -> Option<String> {
    let mut q: BinaryHeap<(isize, Key)> = BinaryHeap::new();

    q.push(with_prio(Key{
        path: "".to_string(),
        pos: (0, 0),
    }));

    while let Some((_, k)) = q.pop() {
        if k.pos == (3, 3) { return Some(k.path) }

        let ds = dirs(key, &k.path);
        let dcs = ['U', 'D', 'L', 'R'];
        for (ix, (dx, dy)) in [(0, -1), (0, 1), (-1, 0), (1, 0)].iter().enumerate() {
            let pos = (k.pos.0 + dx, k.pos.1 + dy);
            if ds[ix] && (0..=3).contains(&pos.0) && (0..=3).contains(&pos.1) {
                q.push(with_prio(Key{
                    path: format!("{}{}", k.path, dcs[ix]),
                    pos,
                }));
            }
        }
    }
    None
}

fn solve_p2(key: &str) -> usize {
    let mut q: BinaryHeap<(isize, Key)> = BinaryHeap::new();

    q.push(with_prio(Key{
        path: "".to_string(),
        pos: (0, 0),
    }));

    let mut r = 0;

    while let Some((_, k)) = q.pop() {
        let ds = dirs(key, &k.path);
        if k.pos == (3, 3) {
            r = std::cmp::max(r, k.path.len());
            continue;
        }

        let dcs = ['U', 'D', 'L', 'R'];
        for (ix, (dx, dy)) in [(0, -1), (0, 1), (-1, 0), (1, 0)].iter().enumerate() {
            let pos = (k.pos.0 + dx, k.pos.1 + dy);
            if ds[ix] && (0..=3).contains(&pos.0) && (0..=3).contains(&pos.1) {
                q.push(with_prio(Key{
                    path: format!("{}{}", k.path, dcs[ix]),
                    pos,
                }));
            }
        }
    }

    r
}

fn main() {
    println!("P1 simple {:?}",   solve_p1("hijkl"));
    println!();
    println!("P1 example1 {:?}", solve_p1("ihgpwlah"));
    println!("P1 example2 {:?}", solve_p1("kglvqrro"));
    println!("P1 example3 {:?}", solve_p1("ulqzkmiv"));
    println!("P1 ans {:?}",      solve_p1("pgflpeqp"));
    println!();
    println!("P2 example1 {:?}", solve_p2("ihgpwlah"));
    println!("P2 example2 {:?}", solve_p2("kglvqrro"));
    println!("P2 example3 {:?}", solve_p2("ulqzkmiv"));
    println!("P2 ans {:?}",      solve_p2("pgflpeqp"));
}
