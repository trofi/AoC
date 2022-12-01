use std::{*, fs::*, io::*, iter::*};

fn get_input(input_file: &str) -> Vec<Vec<String>> {
    let r = BufReader::new(File::open(input_file).unwrap());

    return r.lines().map(|l|
        l.unwrap()
         .split(' ')
         .filter(|e| e != &"|")
         .map(|s| {
             let mut v = Vec::from_iter(s.chars());
             v.sort();
             String::from_iter(v.iter())
         })
         .collect()
    ).collect();
}

fn delta(from: &String, s: &String) -> usize {
    let v = Vec::from_iter(s.chars());
    return from.chars()
               .filter(|c| !v.contains(c))
               .count()
}

fn decode(line: &Vec<String>) -> Vec<String> {
    let mut r = vec!["unknown".to_string(); 10];

    // populate known digits: 1, 4, 7, 8
    for d in line {
        match d.len() {
            2 => r[1] = d.to_string(),
            4 => r[4] = d.to_string(),
            3 => r[7] = d.to_string(),
            7 => r[8] = d.to_string(),
            _ => (),
        }
    }

    // populate immediately derivable by comparison to 1
    for d in line {
        match (d.len(), delta(d,&r[1])) {
            (5, 3) => r[3] = d.to_string(),
            (6, 5) => r[6] = d.to_string(),
            _ => (),
        }
    }
    // populate immediately derivable by comparison to 4
    for d in line {
        match (d.len(), delta(d,&r[4])) {
            (5, 3) => r[2] = d.to_string(),
            (6, 2) => r[9] = d.to_string(),
            _ => (),
        }
    }

    // what left is unambiguous: 0 and 5
    for d in line {
        if r.contains(d) {
            continue
        }

        match d.len() {
            5 => r[5] = d.to_string(),
            6 => r[0] = d.to_string(),
            _ => (),
        }
    }

    return r;
}

fn ix(e: &String, ds: &Vec<String>) -> Option<usize> {
    for (i, v) in ds.iter().enumerate() {
        if v == e {
            return Some(i)
        }
    }
    return None
}

fn main() {
    for input_file in ["example" , "input"] {
        let input = get_input(input_file);


        let ans : usize =
            input.iter().map(|v| {
                let digits = decode(v);

                v.iter()
                 .skip(10)
                 .map(|e| ix(e, &digits).unwrap())
                 .reduce(|val, digit| 10 * val + digit)
                 .unwrap()
             }).sum();

        println!("{} {}", input_file, ans);
    }
}
