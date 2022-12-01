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

fn five_times(cave: &Cave) -> Cave {
    let mut m = Cave::new();
    let (co, ro) = cave.keys().max().unwrap().clone();

    for row in 0..5*(ro + 1) {
        for col in 0..5*(co + 1) {
            let p = (col, row);
            if col <= co && row <= ro {
                m.insert(p, *cave.get(&p).unwrap());
                continue
            }

            let mut ocol = col;
            let mut orow = row;
            if ocol > co { ocol -= co +1; }
            if orow > ro { orow -= ro +1; }

            let mut val = [(ocol, row), (col, orow)].iter()
                                                    .filter_map(|p| m.get(p))
                                                    .max()
                                                    .unwrap() + 1;
            if val > 9 { val = 1; }
            m.insert(p, val);
        }
    }

    return m;
}

fn path(cave: &Cave, ini: &Pos, fini: &Pos) -> usize {
    let mut s = BTreeMap::<Pos, Option<usize>>::new();
    let mut q = BTreeSet::<(usize, Pos)>::new();
    q.insert((0, *ini));

    loop {
        //pop
        let f = q.iter().next().unwrap().clone();
        q.remove(&f);

        let (cost, (c, r)) = f;
        if let Some(Some(v)) = s.get(&(c, r)) {
            if *v < cost {
                continue;
            }
        }

        if &(c, r) == fini { return cost; }

        for np in [(c + 1, r), (c - 1, r), (c, r + 1), (c, r - 1)] {
            if let Some(risk) = cave.get(&np) {
                 q.insert((cost + risk, np));
            }
        }
        s.insert((c, r), Some(cost));
    }
}

fn main() {
    for input_file in ["example", "input"] {
        let input = five_times(&get_input(input_file));

        let ans = path(&input, &(0, 0), input.keys().max().unwrap());

        println!("{}: {:?}", input_file, ans);
    }
}
