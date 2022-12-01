use std::{*, fs::*, io::*, iter::*};

fn get_input(file_name: &str) -> Vec<usize> {
    let r = BufReader::new(File::open(file_name).unwrap());

    return Vec::from_iter(r.lines().map(|l|
        l.unwrap().parse::<usize>().unwrap()));
}

fn main() {
    for input_file in ["example", "input"] {
        let input = get_input(input_file);

        let ans = (0..(input.len()-1)).filter(|&i| input[i] < input[i+1]).count();
        println!("{}: {}", input_file, ans);
    }
}
