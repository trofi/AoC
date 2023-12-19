#[derive(Clone, Copy)]
enum Cmp { LT, GT }

struct Op<'a> {
    var: usize,
    cmp: Cmp,
    val: isize,
    target: &'a str,
}

struct Cmd<'a> {
    label: &'a str,
    ops: Vec<Op<'a>>,
}

const PART_NAMES: &[&str] = &["x", "m", "a", "s"];
type Part = [isize; 4];

fn parse_op(i: &str) -> Op {
    // Parse entries like:
    //     "A"
    //     "R"
    //     "s<537:gd"

    let iv: Vec<&str> = i.split(":").collect();
    if iv.len() == 1 {
        return Op {
            // fake values to be always true
            var: 0,
            cmp: Cmp::LT,
            val: isize::MAX,

            target: iv[0],
        }
    }

    assert!(iv.len() == 2);
    assert!(i.len() >= 5);

    let var = PART_NAMES.iter().position(|n| **n == i[0..1]).expect("var name");
    let cmp: Cmp = match &i[1..2] {
        "<" => Cmp::LT,
        ">" => Cmp::GT,
        _ => panic!("Unhandled comparison {:?} op", &i[1..2]),
    };
    let val: isize = iv[0][2..].parse().expect("a number");

    Op {
        var,
        cmp,
        val,
        target: iv[1],
    }
}

fn parse_cmd(i: &str) -> Cmd {
    // Parse command like "px{a<2006:qkq,m>2090:A,rfg}"
    let iv: Vec<&str> = i.split(&['{', '}']).collect();
    let (label, ops): (&str, &str) = match iv.as_slice() {
        [l, sops, ""] => (l, sops),
        _ => panic!("Unhandled {:?} cmd", iv),
    };

    Cmd {
        label,
        ops: ops.split(",").map(|l| parse_op(l)).collect(),
    }
}

fn parse_part(i: &str) -> Part {
    // Parse input  like "{x=2127,m=1623,a=2188,s=1013}"
    let iv: Vec<&str> = i.split(&['{', '=', ',', '}']).collect();

    match iv.as_slice() {
        ["", "x", sx, "m", sm, "a", sa, "s", ss, ""] => [
            sx.parse().expect("a number"),
            sm.parse().expect("a number"),
            sa.parse().expect("a number"),
            ss.parse().expect("a number"),
        ],
        _ => panic!("Unhandled {:?} part", iv),
    }
}

fn parse_input(i: &str) -> (Vec<Cmd>, Vec<Part>) {
    let (cmdi, parti) = i.split_once("\n\n").expect("two parts");

    let cmds = cmdi.lines().map(|l| parse_cmd(l)).collect();
    let parts = parti.lines().map(|l| parse_part(l)).collect();

    (cmds, parts)
}

fn is_prog_accepting(prog: &[Cmd], p: Part) -> bool {
    let mut rule_no = prog.iter().position(|c| c.label == "in").expect("an in label");
    while rule_no < prog.len() {
        let mut next_rule = prog.len();

        for op in &prog[rule_no].ops {
            let r = match op.cmp {
                Cmp::LT => p[op.var] < op.val,
                Cmp::GT => p[op.var] > op.val,
            };
            if r {
                if op.target == "A" { return true }
                if op.target == "R" { return false }
                next_rule = prog.iter().position(|c| c.label == op.target).expect("a label");
                break;
            }
        }
        rule_no = next_rule;
    }
    panic!("Non-exhaustive rules?");
}

fn solve_p1(i: &str) -> isize {
    let (prog, parts) = parse_input(i);

    parts.into_iter()
         .filter(|p| is_prog_accepting(&prog, *p))
         .map(|p| p.iter().sum::<isize>())
         .sum()
}

fn solve_p2(i: &str) -> isize {
    let (prog, _) = parse_input(i);

    type Cube = [(isize, isize); 4];

    fn step(prog: &[Cmd], wf: &str, c: Cube) -> isize {
        if wf == "R" { return 0 }
        if wf == "A" {
            return c.iter().map(|i| i.1 - i.0 + 1).product();
        }

        let mut nc: Cube = c;
        let mut r: isize = 0;

        let ix = prog.iter().position(|c| c.label == wf).expect("an in label");

        for op in &prog[ix].ops {
            // Hack: I encode unconditional branch that way.
            if op.val == isize::MAX { r += step(&prog, op.target, nc); return r; }

            match op.cmp {
                Cmp::LT => {
                    let mut ct: Cube = nc;
                    ct[op.var].1 = op.val - 1;
                    if ct[op.var].0 < ct[op.var].1 { r += step(&prog, op.target, ct); }
                    nc[op.var].0 = op.val;
                    if nc[op.var].0 >= nc[op.var].1 { return r; }
                },
                Cmp::GT => {
                    let mut ct: Cube = nc;
                    ct[op.var].0 = op.val + 1;
                    if ct[op.var].0 < ct[op.var].1 { r += step(&prog, op.target, ct); }
                    nc[op.var].1 = op.val;
                    if nc[op.var].0 >= nc[op.var].1 { return r; }
                },
            };
        }

        panic!("Non-exhaustive?");
    }

    let c = [
        (1, 4000),
        (1, 4000),
        (1, 4000),
        (1, 4000),
    ];

    step(&prog, "in", c)
}

fn main() {
    let e = std::fs::read_to_string("example").expect("example");
    let i = std::fs::read_to_string("input").expect("input");
    println!("P1 example: {}", solve_p1(&e));
    println!("P1   input: {}", solve_p1(&i));
    println!("P2 example: {}", solve_p2(&e));
    println!("P2   input: {}", solve_p2(&i));
}
