fn parse_input(i: &str) -> Vec<usize> {
    i.lines()
     .map(|e| e.parse::<usize>().expect("number"))
     .collect()
}

fn solve_p1(i: &str) -> usize {
    let v = parse_input(i);

    for i in 0..v.len() {
        for j in (i+1)..v.len() {
            if v[i] + v[j] == 2020 {
                return v[i] * v[j]
            }
        }
    }

    panic!("No P1 solution")
}

fn solve_p2(i: &str) -> usize {
    let v = parse_input(i);

    for i in 0..v.len() {
        for j in (i+1)..v.len() {
            for k in (i+1)..v.len() {
                if v[i] + v[j] + v[k] == 2020 {
                    return v[i] * v[j] * v[k]
                }
            }
        }
    }

    panic!("No P1 solution")
}

fn main() {
    let e = std::fs::read_to_string("example").expect("example");
    let i = std::fs::read_to_string("input").expect("input");
    println!("P1 e: {:?}", solve_p1(&e));
    println!("P1 i: {:?}", solve_p1(&i));
    println!("P2 e: {:?}", solve_p2(&e));
    println!("P2 i: {:?}", solve_p2(&i));
}
