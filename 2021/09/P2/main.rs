use std::{*, collections::*, convert::*, fs::*, io::*};

type Point = (isize, isize);
type Field = HashSet<Point>;

fn get_input(input_file: &str) -> Field {
    let r = BufReader::new(File::open(input_file).unwrap());

    let mut s = Field::new();

    for (row, l) in r.lines().enumerate() {
        for (col, v) in l.unwrap().chars().enumerate() {
            if v != '9' {
                s.insert((col.try_into().unwrap(), row.try_into().unwrap()));
            }
        }
    }

    return s;
}

fn neighbors((col, row) : Point) -> Vec<Point> {
    return [
        (col + 1, row),
        (col - 1, row),
        (col, row + 1),
        (col, row - 1),
    ].to_vec();
}

fn main() {
    for input_file in ["example", "input"] {
        let mut input = get_input(input_file);

        let mut basin_sizes = Vec::new();

        loop {
            if input.len() == 0 { break }

            // pop
            let p = input.iter().next().unwrap().clone();

            let mut basin_size = 0;
            let mut q = [p].to_vec();

            loop {
                match q.pop() {
                  None => break,
                  Some(p) => if input.contains(&p) {
                      input.remove(&p);
                      basin_size += 1;
                      for n in neighbors(p).iter() {
                          q.push(*n);
                      }
                  }
                }
            }

            basin_sizes.push(basin_size);
        }
        basin_sizes.sort_by(|a,b| b.cmp(a));
        let ans : isize = basin_sizes.iter().take(3).product();

        println!("{}: {:?} (basins={})", input_file, ans, basin_sizes.len());
    }
}
