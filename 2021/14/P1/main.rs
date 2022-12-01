use std::{*, cmp::*, collections::*, fs::*, io::*, iter::*};

type VC = Vec<char>;
type Subst = HashMap<VC,VC>;

fn get_input(input_file: &str) -> (VC, Subst) {
    let r = BufReader::new(File::open(input_file).unwrap());

    let mut lines = r.lines();

    let i = Vec::from_iter(lines.next().unwrap().unwrap().chars());
    let mut t = Subst::new();

    for ol in lines {
        let l = ol.unwrap();
        if l == "" { continue }

        let v = Vec::<String>::from_iter(l.split(" -> ").map(|e| e.parse().unwrap()));
        t.insert(Vec::from_iter(v[0].chars()), Vec::from_iter(v[1].chars()));
    }

    return (i, t);
}

fn step(t : &VC, s : &Subst) -> VC {
    let mut o = VC::new();

    for i in 0..t.len()-1 {
        let k = &t[i..i+2];
        let v = s.get(k).unwrap();

        o.push(k[0]);
        o.extend_from_slice(v);
    }
    o.push(*t.iter().last().unwrap());

    return o;
}

fn main() {
    for input_file in ["example", "input"] {
        let input = get_input(input_file);
        let s = &input.1;
        let mut t = input.0.clone();

        for _ in 1..=10 {
            t = step(&t.clone(), &s);
        }
        t.sort();

        let (mut min_c, mut max_c) = (t.len(), 0);
        let mut cc = t[0];
        let mut cnt = 0;

        for c in t.iter() {
            if *c == cc {
                cnt += 1;
            } else {
                min_c = min(min_c, cnt);
                max_c = max(max_c, cnt);

                cnt = 1;
                cc = *c;
            }
        }
        min_c = min(min_c, cnt);
        max_c = max(max_c, cnt);

        let ans = max_c - min_c;

        println!("{}: {:?}", input_file, ans);
    }
}
