mod qc_validate;

use std::{*, cmp, collections::*, fs::*, io::*, iter::*};

type I = (isize, isize);

#[derive(Debug,Eq,Hash,PartialEq,Clone,Copy)]
struct Cubo {
    x: I,
    y: I,
    z: I,
}

#[derive(Debug,Clone,Copy)]
struct Cmd {
    op: bool,
    c: Cubo,
}

fn get_input(input_file: &str) -> Vec<Cmd> {
    let reader = BufReader::new(File::open(input_file).unwrap());

    return Vec::from_iter(
        reader.lines().map(|ol|{

            // "Player 1 starting position: 4"
            let l = ol.expect("no errors");
            let ls = Vec::from_iter(l.split(' '));
            assert!(ls.len() == 2);

            let v = match ls[0] {
                "on" => true,
                "off" => false,
                c => unreachable!("Unexpected command {}", c),
            };

            let vs = Vec::from_iter(
                ls[1].split(",").map(|r|
                    Vec::from_iter(r[2..].split("..").map(|e| {
                        e.parse::<isize>().expect("number") }
                    ))
                )
            );

            Cmd {
                op: v,
                c: Cubo {
                    x: (vs[0][0], vs[0][1]),
                    y: (vs[1][0], vs[1][1]),
                    z: (vs[2][0], vs[2][1]),
                },
            }
        })
    )
}

fn solve_as_p1(input: &[Cmd]) -> isize {
    let mut reactor = HashSet::new();

    for (_inc, cmd) in input.iter().enumerate() {
        for x in cmp::max(-50, cmd.c.x.0)..=cmp::min(50, cmd.c.x.1) {
            for y in cmp::max(-50, cmd.c.y.0)..=cmp::min(50, cmd.c.y.1) {
                for z in cmp::max(-50, cmd.c.z.0)..=cmp::min(50, cmd.c.z.1) {
                    if cmd.op {
                        reactor.insert((x,y,z));
                    } else {
                        reactor.remove(&(x,y,z));
                    }
                }
            }
        }
    }
    return reactor.len() as isize;
}

fn solve_as_p2(_input: &[Cmd]) -> isize {
    unreachable!("TODO");
}

type IS = BTreeSet<I>;

fn flatten(s: &mut IS) -> Vec<I> {
    let mut r = Vec::new();

    while s.len() > 1 {
        // pop two evements, flatten them, push
        // leftovers back.

        let v1 : I = *s.iter().next().unwrap();
        s.remove(&v1);
        let v2 : I = *s.iter().next().unwrap();
        s.remove(&v2);

        //println!("pop: {:?}/{:?}", v1, v2);

        // Non-overlapping:
        //   [ v1 v1 v1 ] [ v2 v2 ... ]
        // Action:
        //   result: [ v1 v1 v1 ]
        //   return: [ v2 v2 ... ]
        if v1.1 < v2.0 {
            //println!("non-overlap");
            r.push(v1);
            s.insert(v2);
            continue;
        }

        // Overlapping, same start:
        //   [ v1 v1 ... v1 ]
        //   [ v2 v2 ... v2 v2 ]
        // Action:
        //   return: [ v1 v1 ... v1 ]
        //   return: [ v1.e + 1 ... v2 v2 ]
        if v1.0 == v2.0 {
            //println!("overlap, same");
            s.insert(v1);
            let ss = v1.1 + 1;
            s.insert((ss, v2.1));
            continue;
        }

        // Overlapping, different start:
        //   [ v1 v1 ...
        //      [ v2 v2 ...
        // Action:
        //   result: [ v1 .. min(v1.e, v2.s) ]
        //   return: [ min(v1.e, v2.s) + 1 .. v1 ] (if non-empty)
        //   return: [ v2 v2 ... ]
        if v1.0 < v2.0 {
            //println!("overlap, different");
            let e = cmp::min(v1.1, v2.0 - 1);
            r.push((v1.0, e));
            if e + 1 <= v1.1 {
                s.insert((e + 1, v1.1));
            }
            s.insert(v2);
            continue;
        }

        unreachable!("Unexpected state: {:?}/{:?}", v1, v2);
    }

    // push rest of the list (0 or 1 element)
    for v in s.iter() {
        r.push(*v);
    }

    return r;
}

fn main() {
    println!("P1 did:\nexample: 590784\nexample_p2: 474140\ninput: 570915\n");

    if false {
        qc_validate::qc_validate();
    }

    for input_file in ["example_p1", "example", "input"] {
        let input = get_input(input_file);

        let mut xs = IS::new();
        let mut ys = IS::new();
        let mut zs = IS::new();

        for i in &input {
            let & c = &i.c;
            xs.insert(c.x);
            ys.insert(c.y);
            zs.insert(c.z);
        }

        println!("{}: unflattened: x/y/z = {}/{}/{}", input_file, xs.len(), ys.len(), zs.len());

        let xv = flatten(&mut xs);
        let yv = flatten(&mut ys);
        let zv = flatten(&mut zs);

        println!("{}: flattened: x/y/z = {}/{}/{}", input_file, xv.len(), yv.len(), zv.len());

        let mut field = HashSet::<(usize, usize, usize)>::new();

        for (ix, i) in input.iter().enumerate() {
            let & c = &i.c;

            let xb = xv.binary_search_by(|probe| probe.0.cmp(&c.x.0)).unwrap();
            let xe = xv.binary_search_by(|probe| probe.1.cmp(&c.x.1)).unwrap();

            let yb = yv.binary_search_by(|probe| probe.0.cmp(&c.y.0)).unwrap();
            let ye = yv.binary_search_by(|probe| probe.1.cmp(&c.y.1)).unwrap();

            let zb = zv.binary_search_by(|probe| probe.0.cmp(&c.z.0)).unwrap();
            let ze = zv.binary_search_by(|probe| probe.1.cmp(&c.z.1)).unwrap();

            for x in xb..=xe {
                for y in yb..=ye {
                    for z in zb..=ze {
                        if i.op {
                            field.insert((x,y,z));
                        } else {
                            field.remove(&(x,y,z));
                        }
                    }
                }
            }
            println!("{}: pulled {}/{}", input_file, ix + 1, input.len());
        }

        let vol : isize = field.iter().map(|(xi, yi, zi)|
            (1 + xv[*xi].1 - xv[*xi].0) *
            (1 + yv[*yi].1 - yv[*yi].0) *
            (1 + zv[*zi].1 - zv[*zi].0)
        ).sum();
        println!("{}: size={}", input_file, vol);

        //println!("{}: p1={}/p2={}", input_file, solve_as_p1(&input), solve_as_p2(&input));
        println!("{}: p1={}", input_file, solve_as_p1(&input));
        //println!("{}: p2={}", input_file, solve_as_p2(&input));
    }
}
