#[derive(Debug)]
struct Range {
    dst: usize,
    src: usize,
    len: usize,
}

#[derive(Debug)]
struct Map<'a> {
    from: &'a str,
    to: &'a str,
    ranges: Vec<Range>,
}

impl Map<'_> {
    fn xlate(self: &Self, v: usize) -> usize {
        for r in self.ranges.iter() {
            if v >= r.src && v < r.src + r.len {
                return v - r.src + r.dst;
            }
        }
        v
    }
}

#[derive(Debug)]
struct Almanac<'a> {
    seeds: Vec<usize>,
    maps: Vec<Map<'a>>,
}

fn parse_input(i: &str) -> Almanac {
    let iv: Vec<&str> = i.split("\n\n").collect();
    assert!(iv.len() >= 1);

    let seeds: Vec<usize> = {
        let iv: Vec<&str> = iv[0].split(&[':', ' ']).collect();
        assert!(iv[0] == "seeds");
        assert!(iv[1] == "");
        iv[2..].into_iter()
               .map(|e| e.parse().expect("a number"))
               .collect()
    };

    let maps: Vec<Map> = {
        iv[1..].into_iter().map(|l| {
            let lv: Vec<&str> = l.lines().collect();
            assert!(lv.len() >= 1);
            let hv: Vec<&str> = lv[0].split(&['-', ' ', ':']).collect();
            match hv.as_slice() {
                [from, "to", to, "map", ""] => Map{
                    from,
                    to,
                    ranges: lv[1..].into_iter().map(|l| {
                        let lv: Vec<&str> = l.split(' ').collect();
                        match lv.as_slice() {
                            [d, s, l] => Range{
                                dst: d.parse().expect("a number"),
                                src: s.parse().expect("a number"),
                                len: l.parse().expect("a number"),
                            },
                            _ => panic!("Unhandled {:?} range", lv),
                        }
                    }).collect(),
                },
                _ => panic!("Unhandled {:?} header", hv),
            }
        }).collect()
    };

    Almanac{seeds,maps}
}

fn solve_p1(i: &str) -> usize {
    let alm = parse_input(i);

    let mut r = usize::MAX;

    for s in alm.seeds.into_iter() {
        let mut pos: &str = &"seed";
        let mut v: usize = s;

        while pos != "location" {
            for m in alm.maps.iter() {
                if m.from != pos { continue }
                v = m.xlate(v);
                pos = m.to;
            }
        }
        assert!(pos == "location");
        r = std::cmp::min(r, v);
    }

    r
}

fn solve_p2_naive(i: &str) -> usize {
    let alm = parse_input(i);

    let mut r = usize::MAX;

    for p in alm.seeds.chunks(2) {
        let (ss, sl) = match p {
            [ss, sl] => (ss, sl),
            _ => panic!("Unpandled {:?} chunk", p),
        };
        for s in *ss..(*ss+*sl) {
            let mut pos: &str = &"seed";
            let mut v: usize = s;

            while pos != "location" {
                for m in alm.maps.iter() {
                    if m.from != pos { continue }
                    v = m.xlate(v);
                    pos = m.to;
                }
            }
            assert!(pos == "location");
            r = std::cmp::min(r, v);
        }
    }

    r
}

fn main() {
    let e = std::fs::read_to_string("example").expect("example");
    let i = std::fs::read_to_string("input").expect("input");
    println!("P1 example: {:?}", solve_p1(&e));
    println!("P1   input: {:?}", solve_p1(&i));
    // naive solution takes about 2 minutes on my machine:
    println!("P2 example: {:?}", solve_p2_naive(&e));
    println!("P2   input: {:?}", solve_p2_naive(&i));
}
