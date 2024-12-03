// run as:
//   $ re2rust main.re -o main.rs && rustc -O main.rs && ./main

#[derive(Debug)]
enum Cmd {
    Mul(isize, isize),
    Do,
    Dont,
}

fn s2n(str: &[u8]) -> isize {
    String::from_utf8_lossy(str).parse::<isize>().expect("a number")
}

fn lex(yyinput: &[u8]) -> Vec<Cmd> {
  assert_eq!(yyinput.last(), Some(&0)); // our grammar requires the terminator

  let mut r: Vec<Cmd> = Vec::new();

  let mut yycursor: usize = 0;
  let mut yymarker: usize = 0;

  /*!svars:re2c format = 'let mut @@ : usize;\n';*/
  /*!stags:re2c format = 'let mut @@ : usize = 0;\n';*/

  'lex: loop {
    /*!re2c
      re2c:define:YYCTYPE = u8;
      re2c:yyfill:enable = 0;
      re2c:tags = 1;
      re2c:unsafe = 0;
      // since re2c-4:
      re2c:api = simple;

      NUM = [0-9]{1,3};

      "mul(" @lns NUM @lne "," @rns NUM @rne ")"  {
        let ln = s2n(&yyinput[lns..lne]);
        let rn = s2n(&yyinput[rns..rne]);
        r.push(Cmd::Mul(ln, rn));
        continue 'lex;
      }

      "do()" {
        r.push(Cmd::Do);
        continue 'lex;
      }
      "don't()" {
        r.push(Cmd::Dont);
        continue 'lex;
      }


      [\x00] { break 'lex; }
      * { continue 'lex; }
    */
  }

  r
}

fn solve_p1(i: &str) -> isize {
    let prog = lex(i.as_bytes());

    let mut r = 0;

    for c in &prog {
        r += match c {
            Cmd::Mul(l, r) => l * r,
            Cmd::Do => 0,
            Cmd::Dont => 0,
        }
    }

    r
}

fn solve_p2(i: &str) -> isize {
    let prog = lex(i.as_bytes());

    let mut r = 0;
    let mut m = 1;

    for c in &prog {
        r += m * match c {
            Cmd::Mul(l, r) => l * r,
            Cmd::Do =>   { m = 1; 0 },
            Cmd::Dont => { m = 0; 0 },
        }
    }

    r
}

fn main() {
    let mut e = std::fs::read_to_string("example").expect("example");
    e.push('\0'); // for re2c's sentinel
    let mut i = std::fs::read_to_string("input").expect("input");
    i.push('\0'); // for re2c's sentinel
    let mut e2 = std::fs::read_to_string("example2").expect("example");
    e2.push('\0'); // for re2c's sentinel
    println!("P1 e: {}", solve_p1(&e));
    println!("P1 i: {}", solve_p1(&i));
    println!("P2 e: {}", solve_p2(&e2));
    println!("P2 i: {}", solve_p2(&i));
}

// vim: ft=rust
