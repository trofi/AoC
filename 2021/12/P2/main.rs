use std::{*, collections::*, fs::*, io::*, iter::*};

type Edges = HashMap<String, Vec<String>>;
type History = HashSet<String>;

fn get_input(input_file: &str) -> Edges {
    let r = BufReader::new(File::open(input_file).unwrap());

    let mut m = Edges::new();

    for ol in r.lines() {
        let l = ol.unwrap();
        let vs = Vec::from_iter(l.split('-'));
        let (f, t) = (vs[0].to_string(), vs[1].to_string());

        if f != "end" && t != "start" {
            match m.get_mut(&f) {
                Some(v) => { v.push(t.clone()); v.sort()},
                None => { m.insert(f.clone(), [t.clone()].to_vec()); ()}
            }
        }

        if f != "start" && t != "end" {
            match m.get_mut(&t) {
                Some(v) => { v.push(f); v.sort()},
                None => { m.insert(t, [f].to_vec()); ()}
            }
        }
    }

    return m;
}

fn explore(edges: &Edges, visited: &History, c: &String, can_again: bool) -> usize {
    if c == "end" { return 1; }

    let mut ca = can_again;
    if c.chars().next().unwrap().is_lowercase() && visited.contains(c) {
        if ca { ca = false; }
        else { return 0; }
    }

    let mut new_visited = visited.clone();
    new_visited.insert(c.clone());

    let mut paths = 0;

    match edges.get(c) {
        None => (),
        Some(cs) => for next_c in cs.iter() {
            paths += explore (edges, &new_visited, next_c, ca);
        }
    }

    return paths;
}

fn main() {
    for input_file in ["example", "input"] {
        let input = get_input(input_file);

        let ans = explore(&input, &History::new(), &"start".to_string(), true);

        println!("{}: {:?}; input", input_file, ans);
    }
}
