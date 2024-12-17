use std::collections::HashSet;

type I = isize;

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
struct Cpu {
    pc: usize,
    regs: [I; 3],
}

#[derive(Copy, Clone, Debug)]
enum Combo {
    Reg(usize),
    Lit(I),
}

const A: usize = 0;
const B: usize = 1;
const C: usize = 2;

#[derive(Copy, Clone, Debug)]
enum Cmd {
    Adv(Combo),
    Bxl(I),
    Bst(Combo),
    Jnz(usize),
    Bxc,
    Out(Combo),
    Bdv(Combo),
    Cdv(Combo),
}

fn decode_combo(o: I) -> Combo {
    match o {
        0..=3 => Combo::Lit(o),
        4..=6 => Combo::Reg((o - 4) as usize),
        _ => panic!("Unhandled operand {:?}", o),
    }
}

fn decode_cmd(cmd: &[I]) -> Cmd {
    assert_eq!(cmd.len(), 2);

    match cmd[0] {
        0 => Cmd::Adv(decode_combo(cmd[1])),
        1 => Cmd::Bxl(cmd[1]),
        2 => Cmd::Bst(decode_combo(cmd[1])),
        3 => Cmd::Jnz(cmd[1] as usize),
        4 => Cmd::Bxc,
        5 => Cmd::Out(decode_combo(cmd[1])),
        6 => Cmd::Bdv(decode_combo(cmd[1])),
        7 => Cmd::Cdv(decode_combo(cmd[1])),
        _ => panic!("Unhandled opcode {:?}", cmd[0]),
    }
}

fn parse_input(i: &str) -> (Cpu, Vec<I>) {
    let (cpu_s, prog_s) = i.split_once("\n\n").expect("double sep");

    let mut cpu = Cpu{
        pc: 0,
        regs: [0; 3],
    };

    for l in cpu_s.lines() {
        let p = l.split_once(": ").expect("colon sep");
        match p {
            ("Register A", sn) => { cpu.regs[A] = sn.parse::<I>().expect("a number (reg A)"); },
            ("Register B", sn) => { cpu.regs[B] = sn.parse::<I>().expect("a number (reg B"); },
            ("Register C", sn) => { cpu.regs[C] = sn.parse::<I>().expect("a number (reg C)"); },
            _ => panic!("Unexpected {:?} cpu initializer", p),
        }
    }

    assert!(&prog_s[..9] == "Program: ");
    let prog: Vec<I> = prog_s[9..]
        .trim()
        .split(',')
        .map(|e| e.parse::<I>().expect("a number (program)"))
        .collect();

    (cpu, prog)
}

fn get_combo(cpu: &Cpu, co: Combo) -> I {
    match co {
        Combo::Lit(v) => v,
        Combo::Reg(rn) => cpu.regs[rn],
    }
}

fn run_cpu(cpu: &mut Cpu, prog: &[I]) -> Vec<I> {
    let mut r = Vec::new();

    let mut visited: HashSet<Cpu> = HashSet::new();

    while cpu.pc < prog.len() {
        if visited.contains(&cpu) { return vec![] }
        visited.insert(cpu.clone());

        let cmd = decode_cmd(&prog[cpu.pc..cpu.pc+2]);
        //println!("cpu={cpu:?}");
        //println!("cmd={cmd:?}");
        let mut next_pc = cpu.pc + 2;

        match cmd {
            Cmd::Adv(co) => {
                let ov = get_combo(&cpu, co);
                cpu.regs[A] = cpu.regs[A] / 2isize.pow(ov as u32);
            },
            Cmd::Bdv(co) => {
                let ov = get_combo(&cpu, co);
                cpu.regs[B] = cpu.regs[A] / 2isize.pow(ov as u32);
            },
            Cmd::Cdv(co) => {
                let ov = get_combo(&cpu, co);
                cpu.regs[C] = cpu.regs[A] / 2isize.pow(ov as u32);
            },
            Cmd::Out(co) => {
                let ov = get_combo(&cpu, co);
                r.push(ov % 8);
            },
            Cmd::Jnz(o) => {
                if cpu.regs[A] != 0 {
                    next_pc = o as usize;
                }
            },
            Cmd::Bst(co) => {
                let ov = get_combo(&cpu, co);
                cpu.regs[B] = ov % 8;
            },
            Cmd::Bxl(o) => {
                cpu.regs[B] = cpu.regs[B] ^ o;
            },
            Cmd::Bxc => {
                cpu.regs[B] = cpu.regs[B] ^ cpu.regs[C];
            },
        }

        cpu.pc = next_pc;
    }

    //println!("end cpu={cpu:?}");

    r
}

fn solve_p1(i: &str) -> String {
    let (mut cpu, prog) = parse_input(i);

    run_cpu(&mut cpu, &prog).into_iter().map(|e| format!("{e}")).collect::<Vec<_>>().join(",")
}

fn run_p2_program(mut a: I, v: &mut Vec<I>){
    //     0 | Bst(Reg(0))           ; begin: B = A % 8
    //     2 | Bxl(5)                ;        B = B ^ 5
    //     4 | Cdv(Reg(1))           ;        C = A >> B
    //     6 | Adv(Lit(3))           ;        A = A >> 3
    //     8 | Bxc                   ;        B = B ^ C
    //    10 | Bxl(6)                ;        B = B ^ 6
    //    12 | Out(Reg(1))           ;        print(B % 8)
    //    14 | Jnz(0)                ;        if (A != 0) jmp begin
    loop {
        let b = a % 8;
        let b = b ^ 5;
        let c = a >> b;
        a = a >> 3;
        let b = b ^ c;
        let b = b ^ 6;

        v.push(b % 8);

        if a == 0 { break }
    }
}

fn solve_p2(i: &str) -> I {
    let (_cpu, prog) = parse_input(i);

    println!("Disassembly:");
    for (n, cmd) in prog.chunks(2).enumerate() {
        println!("  {:4?} | {:?}", n * 2, decode_cmd(cmd));
    }

    // Here I rely on the exact program written, which is:
    //     0 | Bst(Reg(0))           ; begin: B = A % 8
    //     2 | Bxl(5)                ;        B = B ^ 5
    //     4 | Cdv(Reg(1))           ;        C = A >> B
    //     6 | Adv(Lit(3))           ;        A = A >> 3
    //     8 | Bxc                   ;        B = B ^ C
    //    10 | Bxl(6)                ;        B = B ^ 6
    //    12 | Out(Reg(1))           ;        print(B % 8)
    //    14 | Jnz(0)                ;        if (A != 0) jmp begin

    // Every loop iteration is almost self-sufficient. The only input is
    // yet unconsumed A bits.

    // Every 3 bits of A get mangled and printed (lower to higher).
    // The output depends on current 3 bits and up to 7 extra higher
    // bits.

    // The output is 16 * 3-bit digits, thus the input should probably
    // be of similar size: around 48 bits (or more, if it's not enough).

    // We are restoring digits in reverse order.

    fn brute_p2(a: I, ix: usize, prog: &[I], v: &mut Vec<I>) -> Option<I> {
        if ix == prog.len() { return Some(a) }

        // restore one extra 3-bit digit and match against the suffix
        for d in 0..=7 {
            let a = (a << 3) + d;
            v.clear();
            run_p2_program(a, v);
            if *v != prog[prog.len() - 1 - ix..] { continue }

            if let Some(r) = brute_p2(a, ix + 1, prog, v) {
                return Some(r);
            }
        }

        return None
    }

    let mut v = Vec::new();
    brute_p2(0, 0, &prog, &mut v).expect("a solution")
}

fn main() {
    let e = std::fs::read_to_string("example").expect("example");
    let i = std::fs::read_to_string("input").expect("input");

    println!("P1  e: {:?}", solve_p1(&e));
    println!("P1  i: {:?}", solve_p1(&i));
    println!("P2  i: {:?}", solve_p2(&i));
}
