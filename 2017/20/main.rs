use std::collections::HashMap;
use std::collections::HashSet;
use std::iter::FromIterator;

#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash)]
struct C3 { x: isize, y: isize, z: isize }

impl C3 {
    fn dist(self: &Self) -> isize {
        self.x.abs() + self.y.abs() + self.z.abs()
    }
}

#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash)]
struct V3 { p: C3, v: C3, a: C3 }

fn parse_c3(i: &str) -> C3 {
    let v: Vec<isize> =
        i.split(",")
         .map(|e| e.parse().expect("a number"))
         .collect();
    match v.as_slice() {
        [x,y,z] => C3{x: *x,y: *y,z: *z},
        _ => panic!("Unhandled {:?} coord", v),
    }
}

fn parse_p(i: &str) -> V3 {
    // Assume something like:
    // "p=< 3,0,0>, v=< 2,0,0>, a=<-1,0,0>"
    let v: Vec<C3> = i.split(&['<', '>'])
     .filter(|l| !l.contains("=") && !l.is_empty())
     .map(|l| parse_c3(l.trim()))
     .collect();
    match v.as_slice() {
        [p,v,a] => V3{p: *p,v: *v,a: *a},
        _ => panic!("Unhandled {:?} vector", v),
    }
}

fn solve_p1(i: &str) -> usize {
    let mut ps: Vec<(usize, V3)> = Vec::new();

    for (ix, l) in i.lines().enumerate() {
        let p = parse_p(l);
        ps.push((ix, p));
    }

    // acceleration gets us away fastest
    let mut rps: Vec<((isize, isize, isize), usize)> =
        ps.into_iter()
          .map(|(ix, v)| ((v.a.dist(), v.v.dist(), v.p.dist()), ix))
          .collect();

    rps.sort();
    rps[0].1
}

#[derive(Clone, Debug)]
enum Poly2Sol {
    Any,
    S(Vec<isize>),
}

fn intersect_poly2(s1: &Poly2Sol, s2: &Poly2Sol) -> Poly2Sol {
    match (s1, s2) {
        (Poly2Sol::Any, v) => v.clone(),
        (v, Poly2Sol::Any) => v.clone(),
        (Poly2Sol::S(l), Poly2Sol::S(r)) => {
            let ls = HashSet::<isize>::from_iter(l.iter().cloned());
            let rs = HashSet::<isize>::from_iter(r.iter().cloned());
            Poly2Sol::S(ls.intersection(&rs).cloned().collect())
        },
    }
}

// largest r: r * r <= n
fn isqrt(n: isize) -> isize {
    assert!(n as f64 != (n+1) as f64);
    let v = (n as f64).sqrt() as isize;
    assert!(v * v <= n);
    assert!(n < (v + 1) * (v + 1));
    v
}

fn solve_poly2(a: isize, v_: isize, p_: isize) -> Poly2Sol {
    // We are solving the following equation in integer values:
    //   a * t * (t + 1) / 2 + v_ * t + p_ = 0
    // or
    //   a * t^2 + (2 * v_ + a) * t + 2 * p_ = 0

    let v = 2 * v_ + a;
    let p = 2 * p_;

    if a == 0 {
        if v == 0 {
            if p == 0 {
                return Poly2Sol::Any
            } else {
                return Poly2Sol::S(vec![])
            }
        }

        let t = - p / v;
        if v * t + p == 0 {
            return Poly2Sol::S(vec![t])
        } else {
            return Poly2Sol::S(vec![])
        }
    }

    let d = v * v - 4 * a * p;

    if d < 0 {
        return Poly2Sol::S(vec![])
    }

    let dsqrt = isqrt(d);
    if dsqrt * dsqrt != d {
        return Poly2Sol::S(vec![])
    }

    let t1 = (-v - dsqrt) / (2 * a);
    let t2 = (-v + dsqrt) / (2 * a);

    let mut rs = Vec::<isize>::new();
    for t in [t1, t2] {
        if a * t * (t + 1) + 2 * v_ * t + 2 * p_ == 0 {
            rs.push(t);
        }
    }

   Poly2Sol::S(rs)
}

fn coll(l: &V3, r: &V3) -> Vec<usize> {
    // TODO: can I do [].reduce?
    let sx = solve_poly2(r.a.x - l.a.x, r.v.x - l.v.x, r.p.x - l.p.x);
    let sy = solve_poly2(r.a.y - l.a.y, r.v.y - l.v.y, r.p.y - l.p.y);
    let sz = solve_poly2(r.a.z - l.a.z, r.v.z - l.v.z, r.p.z - l.p.z);
    let r = intersect_poly2(&sx, &intersect_poly2(&sy, &sz));

    match r {
        Poly2Sol::Any => vec![0],
        Poly2Sol::S(v) => v.into_iter()
                           .filter(|e| *e >= 0)
                           .map(|e| e as usize)
                           .collect(),
    }
}

fn solve_p2(i: &str) -> usize {
    let ps: Vec<V3> =
        i.lines()
         .map(|e| parse_p(e))
         .collect();

    // calculate collisions: time of a possible collision if exists
    let mut colls: HashMap<(&V3, &V3), usize> = HashMap::new();
    for lix in 0..ps.len() {
        let l = &ps[lix];
        for rix in (lix+1)..ps.len() {
            let r = &ps[rix];

            let ctimes = coll(l, r);
            for t in ctimes.into_iter() {
                colls.insert((&l, &r), t);
            }
        }
    }

    let mut removed = 0usize;

    while colls.len() > 0 {
        let min_t = colls.values().min().expect("at least one");

        let mut to_remove: HashSet<&V3> = HashSet::new();
        for ((l, r), t) in colls.iter() {
            if t != min_t { continue }

            to_remove.insert(l);
            to_remove.insert(r);
        }

        removed += to_remove.len();

        colls = colls.into_iter()
                     .filter(|((l,r), _)| !to_remove.contains(l) && !to_remove.contains(r))
                     .collect();
    }

    ps.len() - removed
}

fn main() {
    let e = std::fs::read_to_string("example").expect("example");
    let e2 = std::fs::read_to_string("example2").expect("example2");
    let i = std::fs::read_to_string("input").expect("input");
    println!("P1 example: {}", solve_p1(&e));
    println!("P1 input: {}", solve_p1(&i));
    println!("P2 example: {}", solve_p2(&e2));
    println!("P2 input: {}", solve_p2(&i));
}
