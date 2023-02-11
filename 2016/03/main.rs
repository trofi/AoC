fn is_triangle(i: &[usize]) -> bool {
    match i {
        &[a, b, c] => a + b > c && b + c > a && a + c > b,
        _ => panic!("Expetcted 3 components, got {:?}", i)
    }
}

fn parse(i: &str) -> Vec<Vec<usize>> {
    i.lines()
     .map(|l|{
         let tv: Vec<usize> =
             l.split(" ")
              .filter(|e| e.len() > 0)
              .map(|e|
                  e.parse()
                   .expect("number"))
              .collect();
         assert!(tv.len() == 3);
         tv
     })
     .collect()
}

fn solve_p1(i: &str) -> usize {
    parse(i)
     .into_iter()
     .filter(|t| is_triangle(t))
     .count()
}

fn solve_p2(i: &str) -> usize {
    let v = parse(i);
    assert!(v.len() % 3 == 0);

    let mut r: usize = 0;

    for n in 0..(v.len() / 3) {
        let row = n * 3;
        for col in 0..3 {
            if is_triangle(&[v[row][col], v[row+1][col], v[row+2][col]]) {
                r += 1;
            }
        }
    }

    r
}

fn main() {
    let i = std::fs::read_to_string("input").expect("input");
    println!("P1 ans: {}", solve_p1(&i));
    println!("P2 ans: {}", solve_p2(&i));
}
