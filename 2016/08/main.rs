const COLS: usize = 50;
const ROWS: usize = 6;

fn solve(i: &str) -> usize {
    let mut display: [[char; ROWS]; COLS] = [['.'; ROWS]; COLS];

    for l in i.lines() {
        let v: Vec<_> = l.split(&[' ', 'x', '=']).collect();
        match v.as_slice() {
            ["rect", sc, sr] => {
                let cs: usize = sc.parse::<usize>().expect("unsigned");
                let rs: usize = sr.parse::<usize>().expect("unsigned");

                for c in 0..cs {
                    for r in 0..rs {
                        display[c][r] = '#';
                    }
                }
            },
            ["rotate", "column", "", "", sc, "by", sn] => {
                let n: usize = sn.parse::<usize>().expect("unsigned");
                let c: usize = sc.parse::<usize>().expect("unsigned");

                let mut col: [char; ROWS] = ['?'; ROWS];
                // store original
                for r in 0..ROWS {
                    col[r] = display[c][r];
                }

                // write rotated
                for r in 0..ROWS {
                    let rr = (r + n + ROWS) % ROWS;
                    display[c][rr] = col[r];
                }
            },
            ["rotate", "row", "y", sr, "by", sn] => {
                let n: usize = sn.parse::<usize>().expect("unsigned");
                let r: usize = sr.parse::<usize>().expect("unsigned");

                let mut row: [char; COLS] = ['?'; COLS];
                // store original
                for c in 0..COLS {
                    row[c] = display[c][r];
                }

                // write rotated
                for c in 0..COLS {
                    let rc = (c + n + COLS) % COLS;
                    display[rc][r] = row[c];
                }
            },
            v => panic!("Unhandled {:?} command", v)
        }
    }

    let mut res = 0;
    println!("State:");
    for r in 0..ROWS {
        for c in 0..COLS {
            if display[c][r] == '#' {
                res += 1;
            }
            print!("{}", display[c][r]);
        }
        println!();
    }
    res
}

fn main() {
    let e = std::fs::read_to_string("example").expect("example");
    let i = std::fs::read_to_string("input").expect("input");
    println!("example: {}", solve(&e));
    println!("ans: {}", solve(&i));
}
