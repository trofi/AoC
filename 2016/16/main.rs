use std::iter::FromIterator;

fn revert(i: &str) -> String {
    String::from_utf8(i.bytes().rev().collect()).expect("valid utf8")
}

fn invert(i: &str) -> String {
    let i = i.chars().map(|c| match c {
        '0' => '1',
        '1' => '0',
        _   => panic!("Unexpected '{:?}' char", c),
    });
    String::from_iter(i)
}

fn csum(i: &str) -> String {
    let mut csum: String = String::from(i);

    while csum.len() % 2 == 0 {
        csum = String::from_utf8(
            csum.as_bytes()
                .chunks(2)
                .map(|c|
                     match c {
                         b"00" => b'1',
                         b"11" => b'1',
                         b"01" => b'0',
                         b"10" => b'0',
                         _     => panic!("Unexpected {:?} sequence", c),
                     }
                ).collect()).expect("valid u8");
    }
    csum
}

// TODO: solve this only using iterators without
// actual string allcoation. It should be feasible
// as we know all the lengths upfront.
fn solve(i: &str, l: usize) -> String {
    let mut f: String = String::from(i);
    while f.len() < l {
        f = format!("{}0{}", f, revert(&invert(&f)));
    }
    csum(&f[0..l])
}

fn main() {
    println!("csum 110010110100 = {}", csum("110010110100"));
    println!("P1 example {:?}", solve("10000", 20));
    println!("P1 ans {:?}", solve("01110110101001000", 272));
    println!("P1 ans {:?}", solve("01110110101001000", 35651584));
}
