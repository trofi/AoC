use std::fs::read_to_string;
use std::collections::BTreeMap;

#[derive(Debug, PartialEq, Eq)]
enum Token<'a> {
  Number(&'a str),
  String(&'a str),
  Ctl(&'a str),
}

#[derive(Debug, PartialEq, Eq)]
enum JSValue<'a> {
  Number(isize),
  String(&'a str),
  Object(BTreeMap<&'a str, JSValue<'a>>),
  Array(Vec<JSValue<'a>>),
}

fn lex<'a>(s: &'a [u8]) -> Vec<Token<'a>> {
  let mut r: Vec<Token> = Vec::new();

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

      [{}:,[\]] {
        let tok = std::str::from_utf8(&s[token_start..cursor]).unwrap();
        r.push(Token::Ctl(tok));
        continue 'lex;
      }

      [-]?[0-9]+ {
        let tok = std::str::from_utf8(&s[token_start..cursor]).unwrap();
        r.push(Token::Number(tok));
        continue 'lex;
      }

      ["] [a-z]* ["] {
        let tok = std::str::from_utf8(&s[token_start+1..cursor-1]).unwrap();
        r.push(Token::String(tok));
        continue 'lex;
      }

      $ { break 'lex; }
      * {
        let bs: Vec<u8> =
          s[token_start..].iter()
                          .take(10)
                          .cloned()
                          .collect();
        panic!("Unhandled input {:?}",
               std::str::from_utf8(&bs).unwrap());
      }
    */
  }

  r
}

fn solve_p1(i: &[Token]) -> isize {
  let mut r: isize = 0;

  for t in i {
    match t {
      Token::Number(sn) => r += sn.parse::<isize>().expect("number"),
      _ => {},
    }
  }

  r
}

fn parse_object<'a>(i: &'a [Token]) -> (JSValue<'a>, &'a [Token<'a>]) {
  let mut ts: &[Token] = i;
  let mut o: BTreeMap<&str, JSValue> = BTreeMap::new();

  assert!(ts[0] == Token::Ctl("{"));
  ts = &ts[1..];

  loop {
    if ts[0] == Token::Ctl("}") { break }
    let (key, ts_rest) = parse_value(ts);
    assert!(ts_rest[0] == Token::Ctl(":"));
    let (value, ts_rest) = parse_value(&ts_rest[1..]);
    match key {
      JSValue::String(s) => { o.insert(s, value); },
      _                  => panic!("Unexpected key type {:?}", key)
    }

    ts = ts_rest;
    if ts[0] == Token::Ctl(",") {
      ts = &ts[1..];
      continue
    }

    break
  }

  assert!(ts[0] == Token::Ctl("}"));
  ts = &ts[1..];

  (JSValue::Object(o), ts)
}

fn parse_array<'a>(i: &'a [Token]) -> (JSValue<'a>, &'a [Token<'a>]) {
  let mut ts: &[Token] = i;
  let mut a: Vec<JSValue> = Vec::new();

  assert!(ts[0] == Token::Ctl("["));
  ts = &ts[1..];

  loop {
    if ts[0] == Token::Ctl("]") { break }
    let (value, ts_rest) = parse_value(ts);
    a.push(value);

    ts = ts_rest;
    if ts[0] == Token::Ctl(",") {
      ts = &ts[1..];
      continue
    }

    break
  }

  assert!(ts[0] == Token::Ctl("]"));
  ts = &ts[1..];

  (JSValue::Array(a), ts)
}

fn parse_value<'a>(i: &'a [Token]) -> (JSValue<'a>, &'a [Token<'a>]) {
  match &i[0] {
    Token::Number(ns) => (JSValue::Number(ns.parse().expect("number")), &i[1..]),
    Token::String(s) => (JSValue::String(s), &i[1..]),
    Token::Ctl("{") => parse_object(i),
    Token::Ctl("[") => parse_array(i),
    t => panic!("Unexpected {:?} token", t)
  }
}

fn p2_sum(v: &JSValue) -> isize {
  match v {
     JSValue::Number(v) => *v,
     JSValue::String(_) => 0,
     JSValue::Object(o) => {
       if o.values().all(|v| *v != JSValue::String("red")) {
         o.values().map(|e| p2_sum(e)).sum()
       } else { 0 }
     },
     JSValue::Array(a) => a.iter().map(|e| p2_sum(e)).sum()
  }
}

fn solve_p2(i: &[Token]) -> isize {
  let (b, rest) = parse_value(i);
  assert!(rest.is_empty());
  p2_sum(&b)
}

fn main() {
  for input_file in ["example", "input"] {
    let i = read_to_string(input_file).expect("all ok");

    let b = lex(i.as_bytes());

    println!("{}: P1 ans: {:?}", input_file, solve_p1(&b));
    println!("{}: P2 ans: {:?}", input_file, solve_p2(&b));
  }
}
