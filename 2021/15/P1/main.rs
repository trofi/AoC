use std::{*, collections::*, convert::*, fs::*, io::*, iter::*};

type Pos=(isize, isize);
type Cave = HashMap<Pos,usize>;

fn get_input(input_file: &str) -> Cave {
    let r = BufReader::new(File::open(input_file).unwrap());

    let mut m = Cave::new();

    for (row, ol) in r.lines().enumerate() {
        for (col, c) in ol.unwrap().chars().enumerate() {
            m.insert((col.try_into().unwrap(), row.try_into().unwrap()), c.to_string().parse::<usize>().unwrap());
        }
    }

    return m;
}

fn path(cave: &Cave, ini: &Pos, fini: &Pos) -> usize {
    let mut q = BTreeSet::<(usize, Pos)>::new();
    q.insert((0, *ini));

    loop {
        //pop
        let f = q.iter().next().unwrap().clone();
        q.remove(&f);

        let (cost, (c, r)) = f;
        if &(c, r) == fini { return cost; }

        for np in [(c + 1, r), (c - 1, r), (c, r + 1), (c, r - 1)] {
            if let Some(risk) = cave.get(&np) {
                 q.insert((cost + risk, np));
            }
        }
    }
}

fn main() {
    for input_file in ["example", "input"] {
        let input = get_input(input_file);

        let ans = path(&input, &(0, 0), input.keys().max().unwrap());

        println!("{}: {:?}", input_file, ans);
    }
}
