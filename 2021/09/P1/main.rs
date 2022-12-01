use std::{*, fs::*, io::*};

fn get_input(input_file: &str) -> Vec<Vec<usize>> {
    let r = BufReader::new(File::open(input_file).unwrap());

    return r.lines().map(|l|
        l.unwrap()
         .chars()
         .map(|c| c.to_string().parse().unwrap())
         .collect()
    ).collect();
}

#[allow(arithmetic_overflow)]
fn main() {
    for input_file in ["example", "input"] {
        let input = get_input(input_file);

        let mut ans = 0;
        for (row, l) in input.iter().enumerate() {
            for (col, val) in l.iter().enumerate() {
                let m = [(usize::MAX, 0), (1, 0), (0, usize::MAX), (0, 1)].iter().all(|(dr, dc)|
                    match input.get(row.overflowing_add(*dr).0) {
                        None => true,
                        Some(nl) => match nl.get(col.overflowing_add(*dc).0) {
                            None => true,
                            Some(nval) => nval > val
                        }
                    }
                );
                if m {
                    ans += val + 1;
                }

            }
        }

        println!("{}: {:?}", input_file, ans);
    }
}
