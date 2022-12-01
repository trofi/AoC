use std::{*, collections::*, convert::*, fs::*, io::*, iter::*};

type Point = (isize, isize);
type Field = HashMap<Point, usize>;

fn get_input(input_file: &str) -> Field {
    let r = BufReader::new(File::open(input_file).unwrap());

    let mut m = Field::new();

    for (row, l) in r.lines().enumerate() {
        for (col, v) in l.unwrap().chars().enumerate() {
            m.insert((col.try_into().unwrap(), row.try_into().unwrap()), v.to_string().parse().unwrap());
        }
    }

    return m;
}

fn neighbors((col, row) : Point) -> Vec<Point> {
    return [
        (col - 1, row - 1), (col - 1, row), (col - 1, row + 1),
        (col,     row - 1),                 (col,     row + 1),
        (col + 1, row - 1), (col + 1, row), (col + 1, row + 1),
    ].to_vec();
}

fn cycle(m : &mut Field) -> usize {
    for (_, v) in m.iter_mut() { *v += 1; }

    // process chained trigegrs
    let mut flashed = HashSet::new();
    let mut queue : Vec<Point> =
        m.iter()
         .filter(|(_, v)| **v > 9)
         .map(|(k,_)| *k)
         .collect();
    loop {
        if queue.len() == 0 { break }

        let p = queue.pop().unwrap().clone();
        if flashed.contains(&p) { continue }
        flashed.insert(p);

        for n in neighbors(p).iter() {
            match m.get_mut(n) {
              Some(nv) => {
                  *nv += 1;
                  if *nv > 9 {
                      queue.push(*n);
                  }
              },
              None => ()
            }
        }
    }

    let mut ans = 0;

    // sweep
    for (_, v) in m.iter_mut() {
        if *v > 9 {
            ans += 1;
            *v = 0;
        }
    }

    return ans;
}

fn main() {
    for input_file in ["example", "input"] {
        let mut input = get_input(input_file);

        for step in 1.. {
            if cycle(&mut input) == input.len() {
                println!("{}: {:?}", input_file, step);
                break;
            }
        }
    }
}
