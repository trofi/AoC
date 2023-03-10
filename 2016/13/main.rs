use std::collections::BinaryHeap;
use std::collections::HashSet;
use std::collections::VecDeque;

type Pos = (isize, isize);

fn f(seed: isize, (x, y): Pos) -> bool {
    let v = x*x + 3*x + 2*x*y + y + y*y + seed;
    v.count_ones() % 2 == 0
}

fn solve_p1(seed: isize, tp: Pos) -> isize {
    #[derive(Eq, Ord, PartialEq, PartialOrd, Debug)]
    struct Key {
        prio: isize,
        steps: isize,
        p: Pos,
    }
    fn get_prio(steps: isize, p: Pos, tp: Pos) -> isize {
        - (steps + (tp.0 - p.0).abs() + (tp.1 - p.1).abs())
    }

    let mut q: BinaryHeap<Key> = BinaryHeap::new();
    q.push(Key{
        prio: get_prio(0, (1,1), tp),
        steps: 0,
        p: (1,1),
    });

    let mut visited: HashSet<Pos> = HashSet::new();

    while let Some(k) = q.pop() {
        if visited.contains(&k.p) { continue }
        visited.insert(k.p);

        if k.p == tp { return k.steps }

        let steps = k.steps + 1;
        for (dx, dy) in [(-1, 0), (1, 0), (0, -1), (0, 1)] {
            let p = (k.p.0 + dx, k.p.1 + dy);
            if p.0 >= 0 && p.1 >= 0 && f(seed, p) {
                q.push(Key{
                    prio: get_prio(steps, p, tp),
                    steps,
                    p,
                });
            }
        }
    }
    panic!("Failed to find the solution!")
}

fn solve_p2(seed: isize, range: usize) -> usize {
    let mut q: VecDeque<(usize, Pos)> = VecDeque::new();
    q.push_back((1, (1, 1)));

    let mut visited: HashSet<Pos> = HashSet::new();

    while let Some(k) = q.pop_front() {
        if visited.contains(&k.1) { continue }
        visited.insert(k.1);

        if k.0 >= range { continue }

        let steps = k.0 + 1;
        for (dx, dy) in [(-1, 0), (1, 0), (0, -1), (0, 1)] {
            let p = (k.1.0 + dx, k.1.1 + dy);
            if p.0 >= 0 && p.1 >= 0 && f(seed, p) {
                q.push_back((steps, p));
            }
        }
    }
    visited.len()
}


fn main() {
    println!("P1 example: {:?}", solve_p1(10, (7, 4)));
    println!("P1 ans: {:?}", solve_p1(1350, (31, 39)));
    println!("P2 ans: {:?}", solve_p2(1350, 51));
}
