extern crate quickcheck;

use crate::*;

impl quickcheck::Arbitrary for Cmd {
    fn arbitrary(g: &mut quickcheck::Gen) -> Cmd {
        let istarts : Vec<isize> = Vec::from_iter(-5..=0);
        let ilens : Vec<isize> = Vec::from_iter(0..=10);

        let x0 = *g.choose(istarts.as_slice()).unwrap();
        let y0 = *g.choose(istarts.as_slice()).unwrap();
        let z0 = *g.choose(istarts.as_slice()).unwrap();

        let xl = *g.choose(ilens.as_slice()).unwrap();
        let yl = *g.choose(ilens.as_slice()).unwrap();
        let zl = *g.choose(ilens.as_slice()).unwrap();
        Cmd{
            op: bool::arbitrary(g),
            c: Cubo{
                x: (cmp::max(-5, x0), cmp::min(5, x0 + xl)),
                y: (cmp::max(-5, y0), cmp::min(5, y0 + yl)),
                z: (cmp::max(-5, z0), cmp::min(5, z0 + zl)),
            },
        }
    }
}

fn prop_p1_p2(input: Vec<Cmd>) -> bool {

    println!("test len={}", input.len());

    let sp1 = solve_as_p1(&input);
    let sp2 = solve_as_p2(&input);

    return sp1 == sp2;
}

pub fn qc_validate() {
    quickcheck::QuickCheck::new().tests(1000).max_tests(1000).quickcheck(prop_p1_p2 as fn(Vec<Cmd>)->bool);
}
