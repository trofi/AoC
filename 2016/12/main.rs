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

#[derive(Debug, Copy, Clone)]
enum Insn {
    Copy(usize, usize),
    Set(isize, usize),
    Inc(usize),
    Dec(usize),
    Jnz(usize, isize),
    Jmp(isize),
}

fn rn2r(rs: &str) -> usize {
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
             ["cpy", s, rd] => {
                 match s.parse::<isize>() {
                     Ok(imm) => Insn::Set(imm, rn2r(rd)),
                     Err(_) => Insn::Copy(rn2r(s), rn2r(rd)),
                 }
             },
             ["inc", r] => Insn::Inc(rn2r(r)),
             ["dec", r] => Insn::Dec(rn2r(r)),
             ["jnz", s, imm] => {
                 match s.parse::<isize>() {
                     Ok(0) => Insn::Jmp(1),
                     Ok(_) => Insn::Jmp(imm.parse().expect("number")),
                     Err(_) => Insn::Jnz(rn2r(s), imm.parse().expect("number")),
                 }
             }
             _ => panic!("Unhandled {:?} instruction", vs)
         }
     }).collect()
}

fn solve(i: &str, b: bool) -> Cpu {
    let pgm = parse(i);
    let mut cpu = Cpu::new();

    if b {
        cpu.regs[2] = 1;
    }

    while cpu.pc >= 0 && (cpu.pc as usize) < pgm.len() {
        let mut delta: isize = 1;
        match pgm[cpu.pc as usize] {
            Insn::Copy(rs, rd) => { cpu.regs[rd] = cpu.regs[rs]; },
            Insn::Set(imm, r)  => { cpu.regs[r] = imm; },
            Insn::Inc(r)       => { cpu.regs[r] += 1; },
            Insn::Dec(r)       => { cpu.regs[r] -= 1; },
            Insn::Jnz(r, imm)  => { if cpu.regs[r] != 0 { delta = imm; } },
            Insn::Jmp(imm)     => { delta = imm; },
        }
        cpu.pc += delta;
    }

    cpu
}

fn main() {
    let e = std::fs::read_to_string("example").expect("example");
    let i = std::fs::read_to_string("input").expect("input");
    println!("P1 example: {:?}", solve(&e, false));
    println!("P1 ans: {:?}", solve(&i, false));
    println!("P2 ans: {:?}", solve(&i, true));
}
