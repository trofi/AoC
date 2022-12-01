use std::{*, collections::*, fs::*, io::*, iter::*};

type VC = [char; 2];
type Subst = HashMap<VC,char>;
type State = HashMap<VC,usize>;

fn get_input(input_file: &str) -> (Vec<char>, Subst) {
    let r = BufReader::new(File::open(input_file).unwrap());

    let mut lines = r.lines();

    let i = Vec::from_iter(lines.next().unwrap().unwrap().chars());
    let mut t = Subst::new();

    for ol in lines {
        let l = ol.unwrap();
        if l == "" { continue }

        // "AB -> C"
        let v = Vec::from_iter(l.chars());
        t.insert([v[0], v[1]], v[6]);
    }

    return (i, t);
}

fn step(m : &State, s : &Subst) -> State {
    let mut o = State::new();

    for (k, v) in m.iter() {
        let c = s.get(k).unwrap();

        o.entry([k[0], *c])
         .and_modify(|e| *e += *v)
         .or_insert(*v);
        o.entry([*c, k[1]])
         .and_modify(|e| *e += *v)
         .or_insert(*v);
    }

    return o;
}

fn main() {
    for input_file in ["example", "input"] {
        let input = get_input(input_file);
        let s = input.0;

        let mut m = State::new();
        for i in 0..s.len()-1 {
            m.entry([s[i], s[i+1]])
             .and_modify(|e| *e += 1)
             .or_insert(1);
        }
        println!("in {:?}: {:?}", s, m);

        for _i in 1..=40 {
            m = step(&m, &input.1);
            //println!("step {}: {:?}", _i, m);
        }

        let mut h = HashMap::<char,usize>::new();
        for (k, v) in m.iter() {
            h.entry(k[0])
             .and_modify(|e| *e += *v)
             .or_insert(*v);
            h.entry(k[1])
             .and_modify(|e| *e += *v)
             .or_insert(*v);
        }
        h.entry(*s.iter().next().unwrap())
         .and_modify(|e| *e += 1);
        h.entry(*s.iter().last().unwrap())
         .and_modify(|e| *e += 1);

        let ans = h.values().max().unwrap() - h.values().min().unwrap();

        println!("{}: {:?}, max={}, min={}", input_file, ans / 2, h.values().max().unwrap(), h.values().min().unwrap());
    }
}
