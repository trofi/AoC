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

        match m.get_mut(&f) {
            Some(v) => v.push(t.clone()),
            None => { m.insert(f.clone(), [t.clone()].to_vec()); ()}
        }

        match m.get_mut(&t) {
            Some(v) => v.push(f),
            None => { m.insert(t, [f].to_vec()); ()}
        }
    }

    return m;
}

fn explore(edges: &Edges, visited: &History, c: &String) -> usize {
    if visited.contains(c) { return 0; }
    if c == "end" { return 1; }

    let mut new_visited = visited.clone();
    if c.chars().next().unwrap().is_lowercase() {
        new_visited.insert(c.clone());
    }

    let mut paths = 0;

    match edges.get(c) {
        None => (),
        Some(cs) => for next_c in cs.iter() {
            paths += explore (edges, &new_visited, next_c);
        }
    }

    return paths;
}

fn main() {
    for input_file in ["example", "input"] {
        let input = get_input(input_file);

        let ans = explore(&input, &HashSet::new(), &"start".to_string());

        println!("{}: {:?}", input_file, ans);
    }
}
