use std::{*, collections::*, fs::*, io::*, iter::*};

#[derive(Debug)]
struct Cmd {
    op: bool,
    x: (isize, isize),
    y: (isize, isize),
    z: (isize, isize),
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
                x: (vs[0][0], vs[0][1]),
                y: (vs[1][0], vs[1][1]),
                z: (vs[2][0], vs[2][1]),
            }
        })
    )
}

fn main() {
    for input_file in ["example", "example_p2", "input"] {
        let input = get_input(input_file);

        let mut reactor = HashSet::new();

        for (_i, cmd) in input.iter().enumerate() {
            for x in cmp::max(-50, cmd.x.0)..=cmp::min(50, cmd.x.1) {
                for y in cmp::max(-50, cmd.y.0)..=cmp::min(50, cmd.y.1) {
                    for z in cmp::max(-50, cmd.z.0)..=cmp::min(50, cmd.z.1) {
                        if cmd.op {
                            reactor.insert((x,y,z));
                        } else {
                            reactor.remove(&(x,y,z));
                        }
                    }
                }
            }
            //println!("{}: {}: {}", input_file, _i, reactor.len());
        }

        println!("{}: {}", input_file, reactor.len());
    }
}
