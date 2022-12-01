use std::{*, fs::*, io::*, iter::*};

#[derive(Debug)]
enum Rec {
  Literal(usize),
  Rec(usize, Vec<Rec>),
}

fn get_input(input_file: &str) -> Vec<String> {
    let r = BufReader::new(File::open(input_file).unwrap());

    return Vec::from_iter(r.lines().map(|e| e.expect("expect line")));
}

#[derive(Debug)]
struct Decoder<'a> {
    input: &'a str,
    state: usize,
    sbits: usize,
    consumed: usize,
}

fn fill(n: usize, d: &mut Decoder) {
    while d.sbits < n && d.input.len() > 0 {
        d.sbits += 4;
        d.state = (d.state << 4)
                | d.input
                   .chars()
                   .map(|c| usize::from_str_radix(&c.to_string(), 16).expect("a hex digit"))
                   .next()
                   .expect("at least one element");
        d.input = &d.input[1..];
    }
}

fn get_bits(n: usize, d: &mut Decoder) -> Option<usize> {
    fill(n, d);

    if d.sbits < n { return None }

    let v = d.state >> (d.sbits - n);
    d.state = d.state ^ (v << (d.sbits - n));
    d.sbits -= n;
    d.consumed += n;

    Some(v)
}

fn get_literal(d: &mut Decoder) -> usize {
    let mut r = 0;

    loop {
        fill(4, d);
        let chunk = get_bits(5, d).expect("5 bits for literal chunk");
        let next_marker = chunk & 0x10;
        let val = chunk ^ next_marker;
        r = (r << 4) | val;

        if next_marker == 0 { break; }
    }

    return r;
}

fn decode_rec(d: &mut Decoder) -> Option<Rec> {
    let mut args = Vec::new();
    //println!("d={:?}", d);
    let _ver = match get_bits(3, d) {
        None => return None,
        Some(v) => v
    };
    //println!("ver={}", _ver);
    let tid = match get_bits(3, d) {
        None => return None,
        Some(v) => v
    };
    //println!("tid={}", tid);
    match tid {
        4 /* literal */ => {
            let lit = get_literal(d);
            //args = vec![Rec::Literal(lit)];
            return Some(Rec::Literal(lit));
        },
        _ /* op with len */ => {
          let len_tag = match get_bits(1, d) {
              None => return None,
              Some(v) => v
          };
          //println!("len_tag={}", len_tag);
          match len_tag {
            0 /* 15-bit total bit len */=> {
                let bit_len = match get_bits(15, d) {
                    None => return None,
                    Some(v) => v
                };
                //println!("bit_len={}", bit_len);
                let expect_consumed = d.consumed + bit_len;
                while d.consumed < expect_consumed {
                    args.push (decode_rec(d).expect("a packet (bits)"));
                }
            },
            1 /* 11-bit subpacket count */ => {
                let packet_count = match get_bits(11, d) {
                    None => return None,
                    Some(v) => v
                };
                //println!("packet_count={}", packet_count);
                for _ in 0..packet_count {
                    args.push (decode_rec(d).expect("a packet (count)"));
                }
            },
            _ => unreachable!("infeasible len_tag={}", len_tag),
          }
        }
    }

    //println!("Decoded: {} {:?}", tid, args);
    Some(Rec::Rec(tid, args))
}

fn decode(s: &String) -> Rec {
    let mut d = Decoder{
        input: s.as_str(),
        state: 0,
        sbits: 0,
        consumed: 0,
    };

    decode_rec(&mut d).expect("single expression")
}

fn eval(rec: &Rec) -> usize {
    //println!("eval({:?})", rec);
    match rec {
        Rec::Literal(v) => *v,
        Rec::Rec(tid, args) => {
            let eas = args.iter().map(|e| eval(e));

            match tid {
              0 | 4 => eas.sum(),
              1 => eas.product(),
              2 => eas.min().expect("at least one min elem"),
              3 => eas.max().expect("at least one max elem"),

              5 => { let v = Vec::from_iter(eas); if v[0] > v[1] { 1 } else { 0 } }
              6 => { let v = Vec::from_iter(eas); if v[0] < v[1] { 1 } else { 0 } }
              7 => { let v = Vec::from_iter(eas); if v[0] == v[1] { 1 } else { 0 } }
              _ => unreachable!("unhandled tid={}", tid)
            }
        }
    }
}

fn main() {
    for input_file in ["example", "input"] {
        let input = get_input(input_file);

        for s in input {
            let d = decode(&s);
            let ans = eval(&d);
            println!("{}: {}", input_file, ans);
        }

    }
}
