use std::collections::HashSet;

#[derive(Clone, Copy, Debug)]
enum Cmd {
    Noop(isize),
    Acc(isize),
    Jmp(isize),
}

fn parse_input(i: &str) -> Vec<Cmd> {
    i.lines().map(|l| {
        let v: Vec<&str> = l.split_whitespace().collect();
        match v.as_slice() {
            ["nop", ns] => Cmd::Noop(ns.parse().expect("a number")),
            ["acc", ns] => Cmd::Acc(ns.parse().expect("a number")),
            ["jmp", ns] => Cmd::Jmp(ns.parse().expect("a number")),
            _ => panic!("Unhandled {:?} op", v),
        }
    }).collect()
}

#[derive(Debug)]
struct Cpu {
    acc: isize,
    ip: isize,
}

impl Cpu {
    fn new() -> Cpu {
        Cpu{
            acc: 0,
            ip: 0,
        }
    }
}

fn solve_p1(i: &str) -> isize {
    let p = parse_input(i);
    let mut cpu: Cpu = Cpu::new();
    let mut seen_ip: HashSet<isize> = HashSet::new();

    loop {
        assert!(cpu.ip >= 0);
        assert!((cpu.ip as usize) < p.len());

        if seen_ip.contains(&cpu.ip) { return cpu.acc }
        seen_ip.insert(cpu.ip);

        let mut delta_ip = 1;
        match p[cpu.ip as usize] {
            Cmd::Noop(_) => {},
            Cmd::Acc(d) => cpu.acc += d,
            Cmd::Jmp(d) => delta_ip = d,
        }
        cpu.ip += delta_ip;
    }
}

enum R {
    Done(isize),
    Loop,
}

fn run(p: &[Cmd]) -> R {
    let mut cpu: Cpu = Cpu::new();
    let mut seen_ip: HashSet<isize> = HashSet::new();

    loop {
        assert!(cpu.ip >= 0);
        if (cpu.ip as usize) == p.len() { return R::Done(cpu.acc) }
        assert!((cpu.ip as usize) < p.len());

        // As there are no control flow constructs in the instruction
        // set trackipg `ip` is enoughto detect loops.
        if seen_ip.contains(&cpu.ip) { return R::Loop }
        seen_ip.insert(cpu.ip);

        let mut delta_ip = 1;
        match p[cpu.ip as usize] {
            Cmd::Noop(_) => {},
            Cmd::Acc(d) => cpu.acc += d,
            Cmd::Jmp(d) => delta_ip = d,
        }
        cpu.ip += delta_ip;
    }
}

fn solve_p2(i: &str) -> isize {
    let p = parse_input(i);

    for (ix, cmd) in p.iter().enumerate() {
        let mut p_patched = p.clone();
        match cmd {
            Cmd::Noop(d) => p_patched[ix] = Cmd::Jmp(*d),
            Cmd::Acc(_) => continue,
            Cmd::Jmp(d) => p_patched[ix] = Cmd::Noop(*d),
        };

        if let R::Done(r) = run(&p_patched) { return r }
    }

    panic!("No solution!");
}

fn main() {
    let e = std::fs::read_to_string("example").expect("example");
    let i = std::fs::read_to_string("input").expect("input");
    println!("P1 e: {:?}", solve_p1(&e));
    println!("P1 i: {:?}", solve_p1(&i));
    println!("P2 e: {:?}", solve_p2(&e));
    println!("P2 i: {:?}", solve_p2(&i));
}
