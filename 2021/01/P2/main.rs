use std::{*, io::*, fs::*, iter::*};

fn main() {
    for input_file in vec!["example", "input"] {
        let r = BufReader::new(File::open(input_file).unwrap());
        let input = Vec::from_iter(r.lines().map(|l|
          l.unwrap().parse::<usize>().unwrap()));

        let mut smoothened = Vec::new();
        for i in 0..(input.len()-2) {
            smoothened.push(input[i] + input[i + 1] + input[i + 2]);
        }

        let mut ans = 0;
        for i in 0..(smoothened.len()-1) {
            if smoothened[i] < smoothened[i+1] {
                ans += 1;
            }
        }
        println!("{}: {}", input_file, ans);
    }
}
