#[derive(Debug)]
struct Cpu {
    regs: [isize; 4],
    pc: isize,
}

impl Cpu {
    fn new() -> Self {
        Cpu {
            regs: [0; 4],
            pc: 0,
        }
    }
}

#[derive(Debug, Clone, Copy)]
enum O {
    Reg(usize),
    Imm(isize),
}

#[derive(Debug, Clone, Copy)]
enum Insn {
    Copy(O, O),
    Jnz(O, O),
    Inc(usize),
    Dec(usize),
    Tgl(usize),
}

fn s2o(s: &str) -> O {
    match s {
        "a" => O::Reg(0),
        "b" => O::Reg(1),
        "c" => O::Reg(2),
        "d" => O::Reg(3),
        _ => {
            if let Ok(imm) = s.parse() {
                O::Imm(imm)
            } else {
                panic!("Unknown {:?} operand", s)
            }
        },
    }
}

fn s2r(rs: &str) -> usize {
    match rs {
        "a" => 0,
        "b" => 1,
        "c" => 2,
        "d" => 3,
        _   => panic!("Unknown {:?} register", rs)
    }
}

fn parse(i: &str) -> Vec<Insn> {
    i.lines()
     .map(|l| {
         let vs: Vec<_> = l.split(' ').collect();
         match vs.as_slice() {
             ["cpy", s, d] => Insn::Copy(s2o(s), s2o(d)),
             ["inc", r]    => Insn::Inc(s2r(r)),
             ["dec", r]    => Insn::Dec(s2r(r)),
             ["jnz", c, t] => Insn::Jnz(s2o(c), s2o(t)),
             ["tgl", r]    => Insn::Tgl(s2r(r)),
             _             => panic!("Unhandled {:?} instruction", vs)
         }
     }).collect()
}

fn solve(i: &str, a: isize) -> Cpu {
    let mut pgm = parse(i);
    let mut cpu = Cpu::new();

    cpu.regs[0] = a;

    while cpu.pc >= 0 && (cpu.pc as usize) < pgm.len() {
        let mut delta: isize = 1;
        let i = pgm[cpu.pc as usize].clone();
        match i {
            Insn::Copy(s, d) => {
                match (s, d) {
                    (O::Reg(rs), O::Reg(rd)) => { cpu.regs[rd] = cpu.regs[rs]; },
                    (O::Imm(is), O::Reg(rd)) => { cpu.regs[rd] = is; },
                    _ => eprintln!("Skipping invalid {:?}", i),
                };
            },
            Insn::Jnz(c, t) => {
                let cv = match c {
                    O::Reg(rc) => cpu.regs[rc],
                    O::Imm(ic) => ic,
                };
                let tv = match t {
                    O::Reg(rt) => cpu.regs[rt],
                    O::Imm(it) => it,
                };
                if cv != 0 { delta = tv; }
            },
            Insn::Inc(r) => { cpu.regs[r] += 1; },
            Insn::Dec(r) => { cpu.regs[r] -= 1; },
            Insn::Tgl(r)           => {
                let tpc = cpu.regs[r] + cpu.pc;
                if tpc >= 0 && (tpc as usize) < pgm.len() {
                    let ti = pgm[tpc as usize].clone();
                    pgm[tpc as usize] = match ti {
                        // 1-arg:
                        Insn::Inc(r) => Insn::Dec(r),
                        Insn::Dec(r) => Insn::Inc(r),
                        Insn::Tgl(r) => Insn::Inc(r),
                        // 2-arg:
                        Insn::Jnz(c, t) => Insn::Copy(c, t),
                        Insn::Copy(s, d) => Insn::Jnz(s, d),
                    };
                }
            },
        }
        cpu.pc += delta;
    }

    cpu
}

fn main() {
    let e = std::fs::read_to_string("example").expect("example");
    let i = std::fs::read_to_string("input").expect("input");
    println!("P1 example: {:?}", solve(&e, 0));
    println!("P1 ans: {:?}", solve(&i, 7));
    println!("P2 ans: {:?}", solve(&i, 12));
}
