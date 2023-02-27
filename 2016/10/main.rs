use std::collections::HashMap;
use std::collections::BinaryHeap;

type Ix = usize;
type Chip = isize;

#[derive(Clone, Copy, Eq, Hash, PartialEq, Ord, PartialOrd, Debug)]
enum Sink {
    Bot(Ix),
    Out(Ix),
}

#[derive(Clone, Copy)]
enum Insn {
    Put(Chip, Sink),
    Decl(Ix, Sink, Sink),
}

fn parse_sink(vi: &[&str]) -> Sink {
    match vi {
        ["bot",    bs] => Sink::Bot(bs.parse().expect("bot number")),
        ["output", os] => Sink::Out(os.parse().expect("out number")),
        _ => panic!("Uknown {:?} sink", vi),
    }
}

fn parse(i: &str) -> Vec<Insn> {
    i.lines()
     .map(|l| {
         let wv: Vec<_> = l.split(' ').collect();
         match wv.as_slice() {
             ["value", vs,
              "goes", "to", st, ss] =>
                 Insn::Put(
                     vs.parse().expect("value number"),
                     parse_sink(&[st, ss])),
             ["bot", bs, "gives", "low", "to",
              slt, sls, "and", "high", "to",
              sht, shs] =>
                  Insn::Decl(
                     bs.parse().expect("bot number"),
                     parse_sink(&[slt, sls]),
                     parse_sink(&[sht, shs])),
             _ => panic!("Unhandled {:?} insn", wv),
         }
     })
     .collect()
}

fn solve(i: &str, v: &(Chip, Chip)) -> (Option<Sink>, Chip) {
    let is = parse(i);

    let mut sink_map: HashMap<Sink, (Sink, Sink)> = HashMap::new();
    let mut state_map: HashMap<Sink, Vec<Chip>> = HashMap::new();

    let mut q: BinaryHeap<Sink> = BinaryHeap::new();

    let enq = |sink: Sink, chip: Chip, state_map: &mut HashMap<Sink, Vec<Chip>>, q: &mut BinaryHeap<Sink>| {
        state_map
            .entry(sink)
            .and_modify(|e| {
                e.push(chip);
                if let Sink::Bot(_) = sink {
                    assert!(e.len() == 2);
                    q.push(sink);
                }
             })
            .or_insert(vec![chip]);
    };
    for i in is {
        match i {
            Insn::Put(chip, sink) => {
                enq(sink, chip, &mut state_map, &mut q);
            },
            Insn::Decl(ix, lsink, hsink) => {
                sink_map.insert(Sink::Bot(ix), (lsink, hsink));
            },
        }
    }

    let mut bix: Option<Sink> = None;

    while !q.is_empty() {
        let sink = q.pop().unwrap();
        let mut vs = state_map.remove(&sink).expect("value");
        vs.sort();
        let node = sink_map.get(&sink).expect("node");

        if vs == vec![v.0, v.1] {
            bix = Some(sink);
        }

        enq(node.0, vs[0], &mut state_map, &mut q);
        enq(node.1, vs[1], &mut state_map, &mut q);
    }

    let mut r = 1;
    for (s, v) in state_map {
        if let Sink::Out(ix) = s {
            if (0..=2).contains(&ix) {
                r *= v[0];
            }
        }
    }

    (bix, r)
}

fn main() {
    let e = std::fs::read_to_string("example").expect("example");
    let i = std::fs::read_to_string("input").expect("input");
    println!("P1/2 example: {:?}", solve(&e, &(2, 5)));
    println!("P1/2 input: {:?}", solve(&i, &(17, 61)));
}
