use std::{*, fs::*, io::*, iter::*};

type Point=(isize, isize);

#[derive(Debug)]
struct Trench {
  xr: (isize, isize),
  yr: (isize, isize),
}

fn get_input(input_file: &str) -> Trench {
    let r = BufReader::new(File::open(input_file).unwrap());

    let mut trench = Trench{ xr: (0,0), yr: (0,0)};

    // parse input like "target area: x=20..30, y=-10..-5"

    for ol in r.lines() {
        let l = ol.expect("expect a line");
        for coords in l.strip_prefix("target area: ").expect("expect prefix").split(", ") {
            let args : Vec<isize> = Vec::from_iter(coords[2..].split("..").map(|e| e.parse::<isize>().expect("expect number")));
            match &coords[0..2] {
                "x=" => trench.xr = (*args.iter().min().expect("at elast one"),
                                     *args.iter().max().expect("at elast one")),
                "y=" => trench.yr = (*args.iter().min().expect("at elast one"),
                                     *args.iter().max().expect("at elast one")),
                _    => unreachable!("unexpected coords={}", coords)
            }
        }
    }

    return trench;
}

fn simulate(v: Point, trench: &Trench) -> Option<Vec<Point>> {
    let mut trace = Vec::new();

    let mut p = (0, 0);
    let mut cv = v;
    loop {
        trace.push(p);

        if p.1 < trench.yr.0 && cv.1 < 0 {
            return None
        }

        if p.0 >= trench.xr.0 && p.0 <= trench.xr.1
           && p.1 >= trench.yr.0 && p.1 <= trench.yr.1 {
           return Some(trace);
        }

        p.0 += cv.0;
        p.1 += cv.1;

        if cv.0 > 0 {
            cv.0 -= 1;
        }
        cv.1 -= 1;
    }
}

fn solve(trench: &Trench) -> isize {
    let ay = [trench.yr.0, trench.yr.1].iter().map(|e| e.abs()).max().expect("at least one");

    let mut count = 0;

    for xv in 0..=trench.xr.1 {
        for yv in -ay..=ay {
            if let Some(_) = simulate((xv, yv), trench) {
                count += 1;
            }
        }
    }
    return count;
}

fn main() {
    for input_file in ["example", "input"] {
        let input = get_input(input_file);

        let ans = solve(&input);

        println!("{}: ans={} input={:?}", input_file, ans, input);
    }
}
