use std::collections::HashMap;

#[derive(Clone)]
enum Cmp {
    Eq, Ne,
    Gt,
    Ge,
    Lt,
    Le,
}

#[derive(Clone)]
enum Pred<'a> {
    If(Cmp, &'a str, isize),
}

#[derive(Clone)]
enum Op<'a> {
    DecI(&'a str, isize),
    IncI(&'a str, isize),
}

#[derive(Clone)]
struct Cmd<'a> {
    pred: Pred<'a>,
    op: Op<'a>,
}

struct Cpu<'a> {
    pc: usize,
    regs: HashMap<&'a str, isize>,
}

impl Cpu<'_> {
    fn new() -> Self {
        Cpu {
            pc: 0,
            regs: HashMap::new(),
        }
    }
}

fn s2i(s: &str) -> isize {
    s.parse().expect("imm32")
}

fn s2pcmp(s: &str) -> Cmp {
    match s {
        "==" => Cmp::Eq,
        "!=" => Cmp::Ne,
        ">"  => Cmp::Gt,
        ">=" => Cmp::Ge,
        "<"  => Cmp::Lt,
        "<=" => Cmp::Le,
        _ => panic!("Unknown predicate {}", s),
    }
}

fn parse<'a>(i: &'a str) -> Vec<Cmd<'a>> {
    i.lines().map(|l|{
        let wv: Vec<&str> = l.split(" ").collect();

        match wv.as_slice() {
            [or, "dec", oimms, "if", pr, pops, pimm] => Cmd {
                pred: Pred::If(s2pcmp(pops), pr, s2i(pimm)),
                op: Op::DecI(or, s2i(oimms)),
            },
            [or, "inc", oimms, "if", pr, pops, pimm] => Cmd {
                pred: Pred::If(s2pcmp(pops), pr, s2i(pimm)),
                op: Op::IncI(or, s2i(oimms)),
            },
            _ => panic!("Unexpected instruction {:?}", wv),
        }
    }).collect()
}

fn solve_p1(i: &str) -> isize {
    let prog = parse(i);
    let mut cpu = Cpu::new();
    while cpu.pc < prog.len() {
        let cmd = prog[cpu.pc].clone();

        let b = match cmd.pred {
            Pred::If(Cmp::Eq, r, imm) => *cpu.regs.entry(r).or_insert(0) == imm,
            Pred::If(Cmp::Ne, r, imm) => *cpu.regs.entry(r).or_insert(0) != imm,
            Pred::If(Cmp::Ge, r, imm) => *cpu.regs.entry(r).or_insert(0) >= imm,
            Pred::If(Cmp::Gt, r, imm) => *cpu.regs.entry(r).or_insert(0) >  imm,
            Pred::If(Cmp::Lt, r, imm) => *cpu.regs.entry(r).or_insert(0) <  imm,
            Pred::If(Cmp::Le, r, imm) => *cpu.regs.entry(r).or_insert(0) <= imm,
        };

        if b {
            match cmd.op {
                Op::DecI(r, imm) => *cpu.regs.entry(r).or_insert(0) -= imm,
                Op::IncI(r, imm) => *cpu.regs.entry(r).or_insert(0) += imm,
            }
        }

        cpu.pc += 1;
    }
    *cpu.regs.values().max().expect("non-empty regs")
}

fn solve_p2(i: &str) -> isize {
    let prog = parse(i);
    let mut cpu = Cpu::new();

    let mut r = isize::MIN;

    while cpu.pc < prog.len() {
        let cmd = prog[cpu.pc].clone();

        let b = match cmd.pred {
            Pred::If(Cmp::Eq, r, imm) => *cpu.regs.entry(r).or_insert(0) == imm,
            Pred::If(Cmp::Ne, r, imm) => *cpu.regs.entry(r).or_insert(0) != imm,
            Pred::If(Cmp::Ge, r, imm) => *cpu.regs.entry(r).or_insert(0) >= imm,
            Pred::If(Cmp::Gt, r, imm) => *cpu.regs.entry(r).or_insert(0) >  imm,
            Pred::If(Cmp::Lt, r, imm) => *cpu.regs.entry(r).or_insert(0) <  imm,
            Pred::If(Cmp::Le, r, imm) => *cpu.regs.entry(r).or_insert(0) <= imm,
        };

        if b {
            match cmd.op {
                Op::DecI(r, imm) => *cpu.regs.entry(r).or_insert(0) -= imm,
                Op::IncI(r, imm) => *cpu.regs.entry(r).or_insert(0) += imm,
            }
            r = std::cmp::max(r, *cpu.regs.values().max().expect("non-empty regs"));
        }

        cpu.pc += 1;
    }

    r
}

fn main() {
    let e = std::fs::read_to_string("example").expect("example");
    let i = std::fs::read_to_string("input").expect("input");

    println!("P1 example: {}", solve_p1(&e));
    println!("P1 ans: {}", solve_p1(&i));

    println!("P2 example: {}", solve_p2(&e));
    println!("P2 ans: {}", solve_p2(&i));
}
