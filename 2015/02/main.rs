use std::fs::read_to_string;

fn solve_p1(i: &str) -> usize {
  i.lines().map(|l| -> usize {
    let v: Vec<usize> = l.split('x').map(|e|
      e.parse::<usize>().expect("a number")
    ).collect();
    let sides = vec![v[0] * v[1], v[1] * v[2], v[0] * v[2]];

    let a  = sides.iter().min().unwrap();
    let sq: usize = 2 * sides.iter().sum::<usize>();

    a + sq
  }).sum()
}

fn solve_p2(i: &str) -> usize {
  i.lines().map(|l| -> usize {
    let v: Vec<usize> = l.split('x').map(|e|
      e.parse::<usize>().expect("a number")
    ).collect();
    let mut sides = vec![v[0], v[1], v[2]];
    sides.sort();

    let r = 2 * (sides[0] + sides[1]);
    let b = sides.iter().product::<usize>();

    r + b
  }).sum()
}


fn main() {
  for input_file in ["example", "input"] {
    let i = read_to_string(input_file).expect("all ok");

    let ans_p1 = solve_p1(&i);
    println!("P1 ans: {:?}", ans_p1);
    let ans_p2 = solve_p2(&i);
    println!("P2 ans: {:?}", ans_p2);
  }
}
