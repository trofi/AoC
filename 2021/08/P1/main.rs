use std::{*, fs::*, io::*};

fn get_input(input_file: &str) -> Vec<Vec<String>> {
    let r = BufReader::new(File::open(input_file).unwrap());

    return r.lines().map(|l|
        l.unwrap()
         .split(' ')
         .map(|s| String::from(s))
         .skip_while(|e| e != "|").skip(1)
         .collect()
    ).collect();
}

fn main() {
    for input_file in ["example", "input"] {
        let input = get_input(input_file);

        let ans = input.into_iter()
                       .flatten()
                       .filter(|e|
                           [2usize,3,4,7].contains(&e.len())
                       )
                       .count();
        println!("{}: {}", input_file, ans);
    }
}
