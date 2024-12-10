use std::collections::HashMap;

fn solve_p1(i: &str) -> usize {
    let mut twos = 0;
    let mut threes = 0;

    for l in i.lines() {
        let mut m: HashMap<char, usize> = HashMap::new();

        for c in l.chars() {
            m.entry(c)
             .and_modify(|v| *v += 1)
             .or_insert(1);
        }

        let mut tw = false;
        let mut th = false;
        for v in m.values() {
            tw |= *v == 2;
            th |= *v == 3;
        }
        if tw { twos +=1 ; }
        if th { threes +=1 ; }
    }

    twos * threes
}

fn solve_p2(i: &str) -> String {
    let v: Vec<&str> = i.lines().collect();

    for i in 0..v.len() {
        for j in (i+1)..v.len() {
            let l = v[i];
            let r = v[j];

            let d = l.chars().zip(r.chars()).filter(|(lc, rc)| lc != rc).count();
            if d == 1 {
                return l.chars().zip(r.chars()).filter(|(lc, rc)| lc == rc).map(|(lc,_)| lc).collect();
            }
        }
    }
    panic!("No solution")
}

fn main() {
    let e = std::fs::read_to_string("example").expect("example");
    let i = std::fs::read_to_string("input").expect("input");
    println!("P1 e: {:?}", solve_p1(&e));
    println!("P1 i: {:?}", solve_p1(&i));
    println!("P2 e: {:?}", solve_p2(&e));
    println!("P2 i: {:?}", solve_p2(&i));
}
