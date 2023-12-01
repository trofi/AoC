// run as:
//   $ re2rust main.re -o main.rs && rustc -O main.rs && ./main

fn solve_p1(i: &str) -> u32 {
    i.lines().map(|l| {
        let ds: Vec<u32> =
            l.chars()
             .filter(|c| c.is_digit(10))
             .map(|c| c.to_digit(10).expect("a digit"))
             .collect();
        assert!(ds.len() > 0);
        let b = ds.iter().next().expect("at least one");
        let e = ds.iter().last().expect("at least one");
        10 * b + e
    }).sum()
}

fn lex_p2(s: &[u8]) -> Vec<u32> {
  let mut r: Vec<u32> = Vec::new();

  let mut cursor: usize = 0;
  let mut marker: usize = 0;
  'lex: loop {
    let token_start: usize = cursor;
    /*!re2c
      re2c:define:YYCTYPE = u8;
      re2c:define:YYPEEK = "*s.get_unchecked(cursor)";
      re2c:define:YYSKIP = "cursor += 1;";
      re2c:define:YYBACKUP    = "marker = cursor;";
      re2c:define:YYRESTORE   = "cursor = marker;";
      re2c:yyfill:enable = 0;
      re2c:eof = 0;
      re2c:define:YYLESSTHAN = "cursor >= s.len()";

      "one"   | "1" { r.push(1); cursor = token_start + 1; continue 'lex; }
      "two"   | "2" { r.push(2); cursor = token_start + 1; continue 'lex; }
      "three" | "3" { r.push(3); cursor = token_start + 1; continue 'lex; }
      "four"  | "4" { r.push(4); cursor = token_start + 1; continue 'lex; }
      "five"  | "5" { r.push(5); cursor = token_start + 1; continue 'lex; }
      "six"   | "6" { r.push(6); cursor = token_start + 1; continue 'lex; }
      "seven" | "7" { r.push(7); cursor = token_start + 1; continue 'lex; }
      "eight" | "8" { r.push(8); cursor = token_start + 1; continue 'lex; }
      "nine"  | "9" { r.push(9); cursor = token_start + 1; continue 'lex; }

      $ { break 'lex; }
      * { continue 'lex; }
    */
  }

  r
}

fn solve_p2(i: &str) -> u32 {
    i.lines().map(|l| {
        let ds: Vec<u32> = lex_p2(l.as_bytes());
        assert!(ds.len() > 0);
        let b = ds.iter().next().expect("at least one");
        let e = ds.iter().last().expect("at least one");
        10 * b + e
    }).sum()
}

fn main() {
    let e = std::fs::read_to_string("example").expect("example");
    let e2 = std::fs::read_to_string("example2").expect("example2");
    let i = std::fs::read_to_string("input").expect("input");
    println!("P1 example: {}", solve_p1(&e));
    println!("P1 input:   {}", solve_p1(&i));
    println!("P2 example: {}", solve_p2(&e2));
    println!("P2 input:   {}", solve_p2(&i));
}

// vim: ft=rust
