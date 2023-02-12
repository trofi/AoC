use std::collections::HashMap;
use std::iter::FromIterator;

// extract "a-b-c123[xyz]" into ("a-b-c", "123", "xyz")
fn extr(i: &str) -> (&str, &str, &str) {
    assert!(&i[i.len() - 1..i.len()] == "]");
    let n = i.find(char::is_numeric).expect("at least one number");
    let ob = i.find('[').expect("at least one '['");

    (&i[0..n], &i[n..ob], &i[ob + 1 .. i.len() - 1])
}

fn extract_code(name: &str) -> String {
   let mut m: HashMap<char, isize> = HashMap::new();
   for c in name.chars() {
       if c == '-' { continue }
       m.entry(c)
         .and_modify(|v| *v += 1)
         .or_insert(1);
   }
   let mut ps: Vec<(isize, char)> =
       m.drain()
        .map(|(k, v)| (-v, k))
        .collect();
   ps.sort();

   String::from_iter(
       ps.into_iter()
         .map(|(_, v)| v))
}

fn solve_p1(i: &str) -> isize {
    let mut r = 0;

    for l in i.lines() {
        let (name, id, csum) = extr(l);
        assert!(csum.len() == 5);
        let id = id.parse::<isize>().expect("number");
        let code = extract_code(name);

        if &code[0..csum.len()] == csum {
            r += id;
        }
    }

    r
}

fn shift(c: char, off: isize) -> char {
    match c {
        '-' => ' ',
        _ if c.is_lowercase() => {
            let base = 'a' as isize;
            let ic: isize = base + (c as isize - base + off) % 26;
            char::from_u32(ic as u32).expect("fits into char")
        },
        _ => panic!("Unexpected {:?} char", c)
    }
}

fn decode(name: &str, off: isize) -> String {
    String::from_iter(name.chars().map(|c| shift(c, off)))
}

fn solve_p2(i: &str) -> isize {
    for l in i.lines() {
        let (name, id, csum) = extr(l);
        assert!(csum.len() == 5);
        let id = id.parse::<isize>().expect("number");
        let code = extract_code(name);

        if &code[0..csum.len()] == csum {
            let decoded = decode(name, id);
            if decoded.contains("northpole") {
                return id
            }
        }
    }
    panic!("No suitable room?!");
}

fn main() {
    let e = std::fs::read_to_string("example").expect("example");
    let i = std::fs::read_to_string("input").expect("input");
    println!("P1 example: {}", solve_p1(&e));
    println!("P1 ans: {}", solve_p1(&i));
    println!("P2 example: {}", decode("qzmt-zixmtkozy-ivhz", 343));
    println!("P2 ans: {}", solve_p2(&i));
}
