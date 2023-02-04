type Reg = usize;
type Offset = isize;

#[derive(Debug, Clone, Copy)]
enum Cmd {
    Half(Reg),
    Triple(Reg),
    Inc(Reg),
    Jmp(Offset),
    JmpIfEven(Reg, Offset),
    JmpIfOne(Reg, Offset),
}

fn parse_reg(r: &str) -> Reg {
    match r {
        "a" => 0,
        "b" => 1,
        _   => panic!("Unexpected reg {}", r),
    }
}

fn parse_offset(o: &str) -> Offset {
    o.parse().expect("offset")
}

fn parse_cmd(l: &[&str]) -> Cmd {
    match l {
        ["hlf", r] => Cmd::Half(parse_reg(r)),
        ["inc", r] => Cmd::Inc(parse_reg(r)),
        ["tpl", r] => Cmd::Triple(parse_reg(r)),

        ["jie", r, "", o] => Cmd::JmpIfEven(parse_reg(r), parse_offset(o)),
        ["jio", r, "", o] => Cmd::JmpIfOne(parse_reg(r), parse_offset(o)),
        ["jmp", o] => Cmd::Jmp(parse_offset(o)),
        _ => panic!("Unsupported {:?} command", l),
    }
}

fn parse_program(i: &str) -> Vec<Cmd> {
    i.lines().into_iter().map(|l| {
        let v: Vec<_> = l.split(&[' ', ',']).collect();
        parse_cmd(&v)
    }).collect()
}

type Regs = [usize; 2];

fn run_program(prog: &[Cmd], mut regs: Regs) -> Regs {
    let mut pc: isize = 0;
    while pc >= 0 && (pc as usize) < prog.len() {
        let mut dpc = 1;
        let cmd = prog[pc as usize];
        match cmd {
            Cmd::Half(r) => regs[r] /= 2,
            Cmd::Triple(r) => regs[r] *= 3,
            Cmd::Inc(r) => regs[r] += 1,

            Cmd::Jmp(o) => dpc = o,
            Cmd::JmpIfEven(r, o) => if regs[r] % 2 == 0 { dpc = o },
            Cmd::JmpIfOne(r, o) => if regs[r] == 1 { dpc = o },
        }
        pc += dpc;
    }
    regs
}

fn solve(i: &str, regs: [usize; 2]) -> usize {
    let program = parse_program(i);
    run_program(&program, regs)[1]
}

fn main() {
    let i = std::fs::read_to_string("input").expect("input");
    println!("P1 ans: {:?}", solve(&i, [0, 0]));
    println!("P2 ans: {:?}", solve(&i, [1, 0]));
}
