use std::{*, collections::*, fs::*, io::*, iter::*};

enum Input {
    Point(isize, isize),
    FoldX(isize),
    FoldY(isize),
}

fn get_input(input_file: &str) -> Vec<Input> {
    let r = BufReader::new(File::open(input_file).unwrap());

    let mut i = Vec::new();

    for ol in r.lines() {
        let l = ol.unwrap();
        if l == "" { continue }
        let v = match (l.contains(","), l.contains("fold along x="), l.contains("fold along y=")) {
            (true, _, _) => {
                let v = Vec::from_iter(l.split(',').map(|e| e.parse().unwrap()));
                Input::Point(v[0], v[1])
            },
            (_, true, _) => Input::FoldX(l.split('=').skip(1).next().unwrap().parse().unwrap()),
            (_, _, true) => Input::FoldY(l.split('=').skip(1).next().unwrap().parse().unwrap()),
            _ => unreachable!()
        };
        i.push(v);
    }

    return i;
}

fn eval(is: &Vec<Input>) -> usize {
    let mut f = HashSet::new();

    for i in is.iter() {
        match i {
            Input::Point(x, y) => {
                f.insert((*x,*y));
            },
            Input::FoldX(x0) => {
                f = HashSet::from_iter(
                    f.iter().map(|(x,y)|
                        if x >= x0 { (2 * x0 - x, *y) }
                        else       { (*x, *y) }
                    )
                );
                return f.len();
            },
            Input::FoldY(y0) => {
                f = HashSet::from_iter(
                    f.iter().map(|(x, y)|
                        if y >= y0 { (*x, 2 * y0 - y) }
                        else       { (*x, *y) }
                    )
                );
                return f.len();
            },
        }
    }

    return f.len();
}

fn main() {
    for input_file in ["example", "input"] {
        let input = get_input(input_file);

        let ans = eval(&input);

        println!("{}: {}", input_file, ans);
    }
}
