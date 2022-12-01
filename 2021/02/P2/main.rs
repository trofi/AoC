use std::{*, io::*, fs::*, iter::*};

fn main() {
    for input_file in vec!["example", "input"] {
        let r = BufReader::new(File::open(input_file).unwrap());

        let (mut h, mut v, mut a) = (0,0,0);

        for ol in r.lines() {
            let l = ol.unwrap();
            let ws = Vec::from_iter(l.split(' '));
            match (ws[0], ws[1].parse::<isize>().unwrap()) {
                ("forward", val) => { h += val; v += a * val },
                ("down", val) => a += val,
                ("up", val) => a -= val,
                _ => unreachable!()
            }
        }

        println!("{}: {}", input_file, h * v);
    }
}
