use std::collections::HashMap;

#[derive(Debug, Clone, Copy)]
enum ElemType {
    FlipFlop,
    Conjunction,
    Broadcast,
    Wire,
}

#[derive(Debug)]
struct Circuit<'a> {
    name2ix: Vec<&'a str>,
    ix2type: Vec<ElemType>,
    ix2wires: Vec<Vec<usize>>,
}

impl<'a> Circuit<'a> {
    fn new() -> Self {
        Self {
            name2ix: Vec::new(),
            ix2type: Vec::new(),
            ix2wires: Vec::new(),
        }
    }
    fn name2ix(self: &mut Self, name: &'a str) -> usize {
        if let Some(ix) = self.name2ix.iter().position(|n| *n == name) {
            return ix;
        }

        self.name2ix.push(name);
        self.ix2type.push(ElemType::Wire);
        self.ix2wires.push(vec![]);

        self.name2ix.len() - 1
    }
}

fn get_type(elem: &str) -> (&str, ElemType) {
    match &elem[0..1] {
        "&" => (&elem[1..], ElemType::Conjunction),
        "%" => (&elem[1..], ElemType::FlipFlop),
        "b" if elem == "broadcaster" => (elem, ElemType::Broadcast),
        _ => panic!("Unhandled {:?} elem type", elem),
    }
}

fn parse_input(i: &str) -> Circuit {
    let mut c = Circuit::new();

    for l in i.lines() {
        let (src, dsts) = l.split_once(" -> ").expect("an arrow");
        let (ename, etype) = get_type(&src);
        let eix = c.name2ix(ename);
        c.ix2type[eix] = etype;

        for d in dsts.split(", ") {
            let dix = c.name2ix(d);
            c.ix2wires[eix].push(dix);
        }
    }

    c
}

// TODO: trait State?

#[derive(Debug)]
struct FlipFlopState {
    val: bool,
}

impl FlipFlopState {
    fn new() -> Self {
        Self {
            val: false,
        }
    }

    fn pulse(self: &mut Self, p: bool) -> Option<bool> {
        if p { return None }

        self.val = !self.val;

        Some(self.val)
    }
}

#[derive(Debug)]
struct ConjunctionState {
    inputs: HashMap<usize, bool>,
}

impl ConjunctionState {
    fn new() -> Self {
        Self {
            inputs: HashMap::new(),
        }
    }

    fn register_input(self: &mut Self, ix: usize) {
        self.inputs.insert(ix, false);
    }

    fn pulse(self: &mut Self, ix: usize, p: bool) -> bool {
        *self.inputs.get_mut(&ix).expect("an input") = p;

        ! self.inputs.values().all(|v| *v)
    }
}

#[derive(Debug)]
struct BroadcasterState {}

impl BroadcasterState {
    fn new() -> Self {
        Self { }
    }
    fn pulse(self: &mut Self) -> bool {
        false
    }
}

#[derive(Debug)]
struct CircuitState<'a, 'b> {
    circuit: &'b Circuit<'a>,
    flipflops: HashMap<usize, FlipFlopState>,
    conjunctions: HashMap<usize, ConjunctionState>,
    broadcasters: HashMap<usize, BroadcasterState>,
    seen_pulses: [usize; 2],
    seen_rx: usize,
    seen_ln: usize,
    seen_dr: usize,
    seen_zx: usize,
    seen_vn: usize,
}

impl<'a, 'b> CircuitState<'a, 'b> {
    fn new(circuit: &'b Circuit<'a>) -> Self {
        let mut flipflops: HashMap<usize, FlipFlopState> = HashMap::new();
        let mut conjunctions: HashMap<usize, ConjunctionState> = HashMap::new();
        let mut broadcasters: HashMap<usize, BroadcasterState> = HashMap::new();

        // walk through the circuit and build initial values
        for (ix, t) in circuit.ix2type.iter().enumerate() {
            match t {
                ElemType::FlipFlop    => { flipflops.insert(ix, FlipFlopState::new()); },
                ElemType::Conjunction => { conjunctions.insert(ix, ConjunctionState::new()); },
                ElemType::Broadcast   => { broadcasters.insert(ix, BroadcasterState::new()); },
                ElemType::Wire => { },
            }
        }

        for (ix, wv) in circuit.ix2wires.iter().enumerate() {
            for iw in wv.iter() {
                match circuit.ix2type[*iw] {
                    ElemType::Wire => { continue },
                    ElemType::FlipFlop => {
                        //let ff = flipflops.get_mut(iw).expect("a flipflop");
                    },
                    ElemType::Conjunction => {
                        let cj = conjunctions.get_mut(iw).expect("a conjunctions");
                        cj.register_input(ix);
                    },
                    ElemType::Broadcast => {
                        //let bc = broadcasters.get_mut(iw).expect("a broadcaster");
                    },
                }
            }
        }

        Self {
            circuit,
            flipflops,
            conjunctions,
            broadcasters,
            seen_pulses: [0; 2],
            seen_rx: 0,
            seen_ln: 0,
            seen_dr: 0,
            seen_zx: 0,
            seen_vn: 0,
        }
    }

    fn pulse(self: &mut Self) {
        let bcix = self.circuit.name2ix.iter().position(|n| *n == "broadcaster").expect("has broadcaster");
        let rxix = self.circuit.name2ix.iter().position(|n| *n == "rx");
        // Extracted by looking at the graphviz dump:
        //   we need low signal for each of "&kj -> rx" inputs.
        let lnix = self.circuit.name2ix.iter().position(|n| *n == "ln");
        let drix = self.circuit.name2ix.iter().position(|n| *n == "dr");
        let zxix = self.circuit.name2ix.iter().position(|n| *n == "zx");
        let vnix = self.circuit.name2ix.iter().position(|n| *n == "vn");
        let invalid_ix = self.circuit.name2ix.len();

        let mut pending_pulses: Vec<(usize, bool, usize)> = vec![(bcix, false, invalid_ix)];

        while pending_pulses.len() > 0 {
            //eprintln!("ppulses={pending_pulses:?}");
            let mut new_pending_pulses: Vec<(usize, bool, usize)> = Vec::new();

            for (ix, p, ifrom) in pending_pulses.into_iter() {
                self.account_pulse(p);

                if rxix == Some(ix) && !p { self.seen_rx += 1; }
                if lnix == Some(ix) && !p { self.seen_ln += 1; }
                if drix == Some(ix) && !p { self.seen_dr += 1; }
                if zxix == Some(ix) && !p { self.seen_zx += 1; }
                if vnix == Some(ix) && !p { self.seen_vn += 1; }

                match self.circuit.ix2type[ix] {
                    ElemType::Wire => { continue },
                    ElemType::FlipFlop => {
                        let ff = self.flipflops.get_mut(&ix).expect("a flipflop");
                        if let Some(r) = ff.pulse(p) {
                            for tix in self.circuit.ix2wires[ix].iter() {
                                new_pending_pulses.push((*tix, r, ix));
                            }
                        }
                    },
                    ElemType::Conjunction => {
                        let cj = self.conjunctions.get_mut(&ix).expect("a conjunctions");
                        let r = cj.pulse(ifrom, p);
                        for tix in self.circuit.ix2wires[ix].iter() {
                            new_pending_pulses.push((*tix, r, ix));
                        }
                    },
                    ElemType::Broadcast => {
                        let bc = self.broadcasters.get_mut(&ix).expect("a broadcaster");
                        let r = bc.pulse();
                        for tix in self.circuit.ix2wires[ix].iter() {
                            new_pending_pulses.push((*tix, r, ix));
                        }
                    },
                }
            }

            pending_pulses = new_pending_pulses;
        }
    }

    fn account_pulse(self: &mut Self, p: bool) {
        self.seen_pulses[p as usize] += 1;
    }
}

fn solve_p1(i: &str) -> usize {
    let c = parse_input(i);

    let mut cs = CircuitState::new(&c);

    for _ in 0..1000 {
        cs.pulse();
    }

    let l = cs.seen_pulses[0];
    let h = cs.seen_pulses[1];

    eprintln!("l={l:?} h={h:?}");

    l * h
}

fn solve_p2(i: &str) -> usize {
    let c = parse_input(i);

    /*
    println!("digraph G {}", '{');
    println!("  node [shape=box]");
    for (n, t) in c.name2type.iter() {
        let ts = match t {
            ElemType::Broadcast => "",
            ElemType::FlipFlop => "%",
            ElemType::Conjunction => "&",
        };
        println!("  {} [label=\"{}:{}\"]", n, ts, n);
    }

    for (s, ts) in c.name2wires.iter() {
        for t in ts.iter() {
            println!("  {} -> {}", s, t);
        }
    }
    println!("{}", '}');
    */

    let mut cs = CircuitState::new(&c);
    let mut prev: (usize, usize, usize, usize, usize) = (0,0,0,0,0);


    let mut periods: Vec<usize> = Vec::new();

    for ix in 1.. {
        cs.pulse();
        let v = (cs.seen_ln, cs.seen_dr, cs.seen_zx, cs.seen_vn, cs.seen_rx);
        if v != prev {
            periods.push(ix);
            prev = v;
            println!("new: {v:?} at ix={ix:?}");
        }
        // happen to be close enough primes without an offset
        if v == (1, 1, 1, 1, 0) { break }
    }

    periods.into_iter().product()
}

fn main() {
    let e = std::fs::read_to_string("example").expect("example");
    let i = std::fs::read_to_string("input").expect("input");
    println!("P1 example: {}", solve_p1(&e));
    println!("P1   input: {}", solve_p1(&i));
    //println!("P2 example: {}", solve_p2(&e));
    println!("P2   input: {}", solve_p2(&i));
}
