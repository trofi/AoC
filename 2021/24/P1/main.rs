use std::{fs::*, io::*, iter::*};

type Val = isize;
type Reg = usize;

#[derive(Debug)]
enum RI {
    R(Reg),
    I(Val),
}
use RI::*;


#[derive(Debug)]
enum Op {
    Add,
    Mul,
    Div,
    Mod,
    Eql,
}
use Op::*;

#[derive(Debug)]
enum Insn {
    Inp(Reg),
    Ari(Op, Reg, RI),
}
use Insn::*;

fn to_reg(r: &str) -> Reg {
    match r {
        "w" => 0,
        "x" => 1,
        "y" => 2,
        "z" => 3,
        _   => unreachable!("Unknown reg {}", r),
    }
}

fn to_ri(r: &str) -> RI {
    match r {
        "w" => R(0),
        "x" => R(1),
        "y" => R(2),
        "z" => R(3),
        _   => I(r.parse::<isize>().expect("digit")),
    }
}

fn get_input(input_file: &str) -> Vec<Insn> {
    let reader = BufReader::new(File::open(input_file).unwrap());

    return Vec::from_iter(
        reader.lines().map(|ol|{
            let l = ol.expect("no errors");
            let ws = Vec::from_iter(l.split(' '));

            match ws[0] {
                "inp" => Inp(to_reg(ws[1])),
                "add" => Ari(Add, to_reg(ws[1]), to_ri(ws[2])),
                "mul" => Ari(Mul, to_reg(ws[1]), to_ri(ws[2])),
                "div" => Ari(Div, to_reg(ws[1]), to_ri(ws[2])),
                "mod" => Ari(Mod, to_reg(ws[1]), to_ri(ws[2])),
                "eql" => Ari(Eql, to_reg(ws[1]), to_ri(ws[2])),
                _     => unreachable!("Uexpected {} instruction", l),
            }
        })
    )
}

#[derive(Debug)]
struct ALU {
    regs: [Val; 4],
}

fn run(s: &mut ALU, program: &[Insn], input: &[Val]) -> bool {
    let mut ix : usize = 0;

    for i in program {
        match i {
            Inp(reg) => {
                if ix >= input.len() { return false; }
                s.regs[*reg] = input[ix];
                ix += 1;
            },
            Ari(op, dreg, ri) => {
                let lval = s.regs[*dreg];
                let rval = match ri { R(sreg) => s.regs[*sreg], I(val) => *val };
                let r = match op {
                    Add => lval + rval,
                    Mul => lval * rval,
                    Div => { if rval == 0 { return false; }; lval / rval },
                    Mod => { if rval <= 0 { return false; }; lval % rval },
                    Eql => if lval == rval { 1 } else { 0 },
                };
                s.regs[*dreg] = r;
            },
        }
    }
    return true;
}

fn calc(input: &[Val]) -> Val {
    let t = Vec::from([
       //ax, ay, dz | ax, ay, dz
       ( 12,  1,  1),
       ( 13,  9,  1),
       ( 12, 11,  1),
       (-13,  6, 26),
       ( 11,  6,  1),
       ( 15, 13,  1),
       (-14, 13, 26),
       ( 12,  5,  1),
       ( -8,  7, 26),
       ( 14,  2,  1),
       ( -9, 10, 26),
       (-11, 14, 26),
       ( -6,  7, 26),
       ( -5,  1, 26),
    ]);

    let mut z = 0;
    for (ix, val) in input.iter().enumerate() {
        let (ax, ay, dz) = t[ix];

        if z % 26 + ax == *val {
            z = z / dz;
        } else {
            z = z / dz * 26 + val + ay;
        }
    }
    return z;
}

/*
 The above is a "stack" machine on the register:
  ADD(v1+1)
  ADD(v2+9)
  ADD(v3+11)
  SWAP_OR_DROP(v4+6) | on -13
  ...

To get a final zero all SWAP_OR_DROP need to be DROPs.
This leads us to the following set of digit equations:

v14 =  v1 - 4
v13 =  v2 + 3
v12 =  v5 - 5
v11 = v10 - 7
 v9 =  v8 - 3
 v7 =  v6 - 1
 v4 =  v3 - 2

From here we can easily derive smallest and largest digits
picking boundaries from v1 (most significant) down to v14.
*/

fn main() {
    let program = get_input("input");

    let inputs = [
        "00000000000000",
        "11111111111111",
        "22222222222222",
        "12345678987654",
        "13579246899999",
        "99999999999999",
        "99979989692475",
        "99979989692465",
        "99979989692465",
        "96979989692495", // max, ok!
        "51316214181141", // min, ok!
    ];

    for i in inputs {

        let mut alu = ALU{
            regs: [0, 0, 0, 0],
        };
        let input = Vec::from_iter(
            i.chars().map(|c| c.to_digit(10).expect("digit") as isize)
        );
        let r = run(&mut alu, program.as_slice(), input.as_slice());
        println!("{}: model? {} failed? {}, state: {:?}, calc: {}", i, alu.regs[3] == 0, !r, alu, calc(&input));
    }
}
