use std::collections::HashMap;
use std::io::Write;
use std::iter::FromIterator;

#[derive(Debug)]
enum Op {
    And,
    Or,
    Xor,
}

#[derive(Debug)]
struct Input<'a> {
    // (name, value)
    wires: Vec<(&'a str, bool)>,
    // (input, input, op, output)
    elems: Vec<(&'a str, &'a str, Op, &'a str)>,
}

fn parse_input(i: &str) -> Input {
    let (ws_s, es_s) = i.split_once("\n\n").expect("wires and elements");

    let wires = ws_s
        .lines()
        .map(|l| {
            let (w, v) = l.split_once(": ").expect("wire and value");
            let bv = match v {
                "0" => false,
                "1" => true,
                _ => panic!("Unexpected wire value"),
            };

            (w, bv)
        })
        .collect();

    let elems = es_s
        .lines()
        .map(|l| {
            let ws: Vec<&str> = l
                .split(&[' ', '-', '>'])
                .filter(|e| *e != "")
                .collect();

            match ws.as_slice() {
                [lw, op_s, rw, ow] => {
                    let op = match *op_s {
                        "AND" => Op::And,
                        "OR" => Op::Or,
                        "XOR" => Op::Xor,
                        _ => panic!("unknown wire op={:?}", op_s),
                    };

                    (*lw, *rw, op, *ow)
                },
                _ => panic!("Unexpected elem description {:?}", ws),
            }
        })
        .collect();

    Input {
        wires,
        elems,
    }
}

fn solve_p1(i: &str) -> usize {
    let i = parse_input(i);

    let mut signals: HashMap<&str, bool> = HashMap::from_iter(i.wires.iter().cloned());

    'again: loop {
        for (l, r, op, out) in &i.elems {
            if signals.contains_key(*out) { continue }

            // propagate newly calculated output
            if let Some(lv) = signals.get(l) {
                if let Some(rv) = signals.get(r) {
                    let ov = match op {
                        Op::And => *lv & *rv,
                        Op::Or => *lv | *rv,
                        Op::Xor => *lv ^ *rv,
                    };
                    signals.insert(*out, ov);
                    continue 'again
                }
            }
        }

        // no updates found
        break
    }

    // get result
    let mut zv: Vec<_> = signals
        .into_iter()
        .filter(|(k, _)| k.starts_with("z"))
        .collect();
    zv.sort();
    zv.into_iter().rev()
      .fold(0, |acc, v| 2 * acc + (v.1 as usize))
}

fn solve_p2(i: &str) -> String {
    let i = parse_input(i);

    eprintln!("output_count: {:?}", i.elems.len());

    // The input is expected to implement adder on
    // x?? + y?? = z??
    //
    // it is implemented as a natural sequence of half-adders against
    // inputs and carries:
    //
    //   half-adder-1:
    //     xyN   =  xN XOR yN
    //     xyC   =  xN AND yN
    //   half-adder-2:
    //      zN   = xyN XOR cN
    //      xycN = xyN AND cN
    //   carry-out:
    //     cN+1  = xyC  OR xycN
    //
    // As it's just 222 nodes I'm dumping it to graphviz and eyeballing
    // the result to make sure the above is correct.

    {
        let mut f = std::fs::File::create("gv.dot").expect("opemend 'gv.dot'");
        f.write(b"digraph G {\n").unwrap();
        f.write(b"    rankdir=\"LR\";\n").unwrap();

        for (l, r, op, o) in &i.elems {
            let op_s = match op {
                Op::And => "AND",
                Op::Or  => "OR",
                Op::Xor => "XOR",
            };
            let node = format!("{l}-{r}-{op_s}-{o}");
            f.write(format!("    \"{l}\"    -> \"{node}\"\n").as_bytes()).unwrap();
            f.write(format!("    \"{r}\"    -> \"{node}\"\n").as_bytes()).unwrap();
            f.write(format!("    \"{node}\" -> \"{o}\"\n").as_bytes()).unwrap();
        }

        f.write(b"}\n").unwrap();
    }

    // Eyeballed the irregular nodes at:
    //     $ dot -Tpng gv.dot > gv.png
    let mut vs = [
        // z06
        "z06", "fhc",
        // z11
        "qhj", "z11",
        // z23
        "mwh", "ggt",
        // z35
        "z35", "hqk",
    ];
    vs.sort();
    vs.join(",")
}

fn main() {
    let e = std::fs::read_to_string("example").expect("example");
    let i = std::fs::read_to_string("input").expect("input");
    println!("P1 e: {:?}", solve_p1(&e));
    println!("P1 i: {:?}", solve_p1(&i));
    println!("P2 i: {:?}", solve_p2(&i));
}
