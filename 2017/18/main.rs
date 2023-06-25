use std::collections::HashMap;
use std::collections::VecDeque;

#[derive(Debug)]
enum Op {
    Set,
    Add,
    Mul,
    Mod,
    Jgz,
}

#[derive(Debug)]
enum ROI<'a> {
    Reg(&'a str),
    Imm(isize),
}

#[derive(Debug)]
enum Cmd<'a> {
    Op(Op, &'a str, ROI<'a>),
    SndR(ROI<'a>),
    RcvR(&'a str),
}

fn parse_roi(i: &str) -> ROI {
    match i.parse::<isize>() {
        Ok(n) => ROI::Imm(n),
        Err(_) => ROI::Reg(i),
    }
}

fn parse_prog(i: &str) -> Vec<Cmd> {
    i.lines().into_iter().map(|l| {
        let vl: Vec<&str> = l.split(" ").collect();
        match vl.as_slice() {
            [sop, rs, rns] => {
                let op = match *sop {
                    "set" => Op::Set,
                    "add" => Op::Add,
                    "mul" => Op::Mul,
                    "mod" => Op::Mod,
                    "jgz" => Op::Jgz,
                    _ => panic!("Unknown op: {}", sop),
                };
                Cmd::Op(op, rs, parse_roi(rns))
            },
            ["snd", rns] => Cmd::SndR(parse_roi(rns)),
            ["rcv", rs] => Cmd::RcvR(rs),
            _ => panic!("Unknown insn {:?}", vl),
        }
    }).collect()
}

#[derive(Debug)]
struct CpuP1<'a> {
    pc: isize,
    regs: HashMap<&'a str, isize>,
    sound: Option<isize>,
}

impl <'a>CpuP1<'_> {
    fn new() -> CpuP1<'a> {
        CpuP1{
            pc: 0,
            regs: HashMap::new(),
            sound: None,
        }
    }
}

fn solve_p1(i: &str) -> isize {
    let prog = parse_prog(i);
    let mut cpu = CpuP1::new();

    loop {
        let mut next_pc: isize = cpu.pc + 1;

        match &prog[cpu.pc as usize] {
            Cmd::Op(op, dr, sroi) => {
                let slv = *cpu.regs.get(dr).unwrap_or(&0);
                let srv = match sroi {
                    ROI::Reg(r) => *cpu.regs.get(r).unwrap_or(&0),
                    ROI::Imm(i) => *i,
                };
                let v = match op {
                    Op::Set => srv,
                    Op::Add => slv + srv,
                    Op::Mul => slv * srv,
                    Op::Mod => slv % srv,
                    Op::Jgz => {
                        if slv > 0 {
                            next_pc = cpu.pc + srv;
                        }
                        slv
                    },
                };
                cpu.regs.insert(dr, v);
            }
            Cmd::SndR(sroi) => {
                let v = match sroi {
                    ROI::Reg(r) => *cpu.regs.get(r).unwrap_or(&0),
                    ROI::Imm(i) => *i,
                };
                cpu.sound = Some(v);
            },
            Cmd::RcvR(dr) => {
                let v = *cpu.regs.get(dr).unwrap_or(&0);
                if v != 0 {
                    return cpu.sound.expect("some sound");
                }
            },
        }

        cpu.pc = next_pc;
    }
}

#[derive(Debug)]
struct CpuP2<'a> {
    pc: isize,
    regs: HashMap<&'a str, isize>,
    q: VecDeque<isize>,
    is_blocked: bool,
}

impl <'a>CpuP2<'_> {
    fn new() -> CpuP2<'a> {
        CpuP2{
            pc: 0,
            regs: HashMap::new(),
            q: VecDeque::new(),
            is_blocked: false,
        }
    }
}

fn solve_p2(i: &str) -> usize {
    let prog = parse_prog(i);
    let mut cpus = vec![CpuP2::new(), CpuP2::new()];
    let cl = cpus.len();
    cpus[0].regs.insert("p", 0);
    cpus[1].regs.insert("p", 1);
    let mut cn = 0;

    // workaround 'jgz 1 <foo>' handling with '1' being a register of
    // value '1'.
    cpus[0].regs.insert("1", 1);
    cpus[1].regs.insert("1", 1);

    let mut r = 0;

    loop {
        if cpus.iter().all(|c| c.is_blocked) {
            break
        }
        if cpus[cn].is_blocked {
            cn = (cn + 1) % cl;
            continue
        }

        let mut next_pc: isize = cpus[cn].pc + 1;

        match &prog[cpus[cn].pc as usize] {
            Cmd::Op(op, dr, sroi) => {
                let slv = *cpus[cn].regs.get(dr).unwrap_or(&0);
                let srv = match sroi {
                    ROI::Reg(r) => *cpus[cn].regs.get(r).unwrap_or(&0),
                    ROI::Imm(i) => *i,
                };
                let v = match op {
                    Op::Set => srv,
                    Op::Add => slv + srv,
                    Op::Mul => slv * srv,
                    Op::Mod => { assert!(slv >= 0); assert!(srv >= 0); slv % srv },
                    Op::Jgz => {
                        if slv > 0 {
                            next_pc = cpus[cn].pc + srv;
                        }
                        slv
                    },
                };
                cpus[cn].regs.insert(dr, v);
            }
            Cmd::SndR(sroi) => {
                let v = match sroi {
                    ROI::Reg(r) => *cpus[cn].regs.get(r).unwrap_or(&0),
                    ROI::Imm(i) => *i,
                };
                let tcn = (cn + 1) % cl;
                cpus[tcn].q.push_back(v);
                cpus[tcn].is_blocked = false;
                if cn == 1 {
                    r += 1;
                }
            },
            Cmd::RcvR(dr) => {
                let tcn = (cn + 1) % cl;
                if let Some(v) = cpus[cn].q.pop_front() {
                    cpus[cn].regs.insert(dr, v);
                    cpus[tcn].is_blocked = false;
                } else {
                    cpus[cn].is_blocked = true;
                    continue
                }
            },
        }

        cpus[cn].pc = next_pc;
        cn = (cn + 1) % cl;
    }

    r
}

fn main() {
    let e = std::fs::read_to_string("example").expect("example");
    let i = std::fs::read_to_string("input").expect("input");
    let e2 = std::fs::read_to_string("example2").expect("example2");

    println!("P1 example: {}", solve_p1(&e));
    println!("P1 ans: {}", solve_p1(&i));
    println!("P2 example: {}", solve_p2(&e2));
    println!("P2 ans: {}", solve_p2(&i));
}
