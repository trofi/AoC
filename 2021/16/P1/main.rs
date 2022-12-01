use std::{*, fs::*, io::*, iter::*};

#[derive(Debug)]
enum Payload {
  Literal(usize),
  UnknownBits(usize),
  UnknownPackets(usize),
}

#[derive(Debug)]
struct Rec {
  ver: usize,
  tid: usize,
  data: Vec<Payload>,
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

fn decode(s: &String) -> Vec<Rec> {
    let mut r = Vec::new();

    let mut d = Decoder{
        input: s.as_str(),
        state: 0,
        sbits: 0,
    };

    loop {
        //println!("d={:?}", d);
        let ver = match get_bits(3, &mut d) {
            None => break,
            Some(v) => v
        };
        //println!("ver={}", ver);
        let tid = match get_bits(3, &mut d) {
            None => break,
            Some(v) => v
        };
        //println!("tid={}", tid);
        match tid {
            4 /* literal */ => {
                let lit = get_literal(&mut d);
                r.push(Rec{ver: ver, tid: tid, data: vec![Payload::Literal(lit)]});
            },
            _ /* op with len */ => {
              let len_tag = match get_bits(1, &mut d) {
                  None => break,
                  Some(v) => v
              };
              //println!("len_tag={}", len_tag);
              match len_tag {
                0 /* 15-bit total bit len */=> {
                    let bit_len = match get_bits(15, &mut d) {
                        None => break,
                        Some(v) => v
                    };
                    /* ignore payload */
                    r.push(Rec{ver: ver, tid: tid, data: vec![Payload::UnknownBits(bit_len)]});
                },
                1 /* 11-bit subpacket count */ => {
                    let packet_count = match get_bits(11, &mut d) {
                        None => break,
                        Some(v) => v
                    };
                    /* ignore payload */
                    r.push(Rec{ver: ver, tid: tid, data: vec![Payload::UnknownPackets(packet_count)]});
                },
                _ => unreachable!("infeasible len_tag={}", len_tag),
              }
            }
        }
    }

    return r;
}

fn main() {
    for input_file in ["example", "input"] {
        let input = get_input(input_file);

        for s in input {
            let d = decode(&s);
            let ans : usize = d.iter().map(|e| e.ver).sum();
            println!("{}: {}", input_file, ans);
        }

    }
}
