use std::collections::HashSet;

type ID = usize;

#[derive(Debug)]
enum Entry {
    Free(usize),
    File(ID, usize),
}

impl Entry {
    fn is_file(self: &Self) -> bool {
        match self {
            Entry::Free(_) => false,
            Entry::File(_, _) => true,
        }
    }
    fn is_free(self: &Self) -> bool {
        match self {
            Entry::Free(_) => true,
            Entry::File(_, _) => false,
        }
    }
    fn len(self: &Self) -> usize {
        match self {
            Entry::Free(l) => *l,
            Entry::File(_, l) => *l,
        }
    }
    fn id(self: &Self) -> usize {
        match self {
            Entry::File(id, _) => *id,
            _ => panic!("Unhandled 'id' for Free entry"),
        }
    }
    fn shrink(self: &mut Self, l: usize) {
        assert!(self.len() >= l);
        match self {
            Entry::Free(len) => { *len -= l; } ,
            Entry::File(_, len) => { *len -= l; },
        }
    }
}

type DiskMap = Vec<Entry>;

fn parse_input(i: &str) -> DiskMap {
    let mut r = DiskMap::new();

    for (i, c) in i.chars().enumerate() {
        let id = i / 2;
        let l: usize = c.to_digit(10).expect("a digit") as usize;
        let e = match i % 2 {
            0 => Entry::File(id, l),
            1 => Entry::Free(l),
            _ => panic!("Unhandled value i={:?}", i),
        };
        r.push(e);
    }

    r
}

fn csum(dm: &DiskMap) -> usize {
    let mut r: usize = 0;
    let mut p: usize = 0;

    for e in dm {
        match e {
            Entry::File(id, len) => {
                for ix in 0..*len {
                    r += id * (p + ix);
                }
                p += len;
            },
            Entry::Free(len) => { p += len; },
        }
    }

    r
}

fn compact_p1(dm: &mut DiskMap) {
    assert!(dm.len() >= 1);

    let mut bp: usize = 0;
    let mut ep: usize = dm.len() - 1;

    loop {
        if bp >= ep { break }
        // forward scan beginnning for Free
        if dm[bp].is_file() { bp += 1; continue }
        // backward scan end for non-empty File
        if dm[ep].is_free() || dm[ep].len() == 0 { ep -= 1; continue }

        // if Free is less than Entry move only part of it
        if dm[bp].len() <= dm[ep].len() {
            let l: usize = dm[bp].len();
            dm[bp] = Entry::File(dm[ep].id(), l);
            dm[ep].shrink(l);
            dm.insert(ep + 1, Entry::Free(l));
            bp += 1;
            continue
        }

        // dm[bp].len() > dm[ep].len()
        let l: usize = dm[ep].len();
        dm[bp].shrink(l);
        dm[ep].shrink(l);
        dm.insert(bp, Entry::File(dm[ep].id(), l));
        bp += 1;
        ep += 1;
    }
}

fn compact_p2(dm: &mut DiskMap) {
    assert!(dm.len() >= 1);

    let mut bp: usize = 0;
    let mut ep: usize = dm.len() - 1;

    let mut moved: HashSet<ID> = HashSet::new();

    loop {
        if ep == 0 { break }

        if bp >= ep {
            ep -= 1;
            bp = 0;
            continue
        }
        // forward scan beginnning for Free
        if dm[bp].is_file() { bp += 1; continue }
        // backward scan end for non-empty File
        if dm[ep].is_free() { ep -= 1; continue }
        // skip already moved files
        if moved.contains(&dm[ep].id()) { ep -= 1; continue }
        // skip small Free holes
        if dm[bp].len() < dm[ep].len() {
            bp += 1;
            continue
        }

        // dm[bp].len() >= dm[ep].len()
        let l: usize = dm[ep].len();
        let id = dm[ep].id();
        dm[bp].shrink(l);
        dm[ep] = Entry::Free(l);
        dm.insert(bp, Entry::File(id, l));
        moved.insert(id);
        // Look for free slots from the beginning:
        bp = 0;
        ep += 1;
    }
}

fn solve_p1(i: &str) -> usize {
    let mut dm = parse_input(i);

    compact_p1(&mut dm);

    csum(&dm)
}

fn solve_p2(i: &str) -> usize {
    let mut dm = parse_input(i);

    compact_p2(&mut dm);

    csum(&dm)
}

fn main() {
    let e = std::fs::read_to_string("example").expect("example");
    let i = std::fs::read_to_string("input").expect("input");
    println!("P1 e: {:?}", solve_p1(&e));
    println!("P1 i: {:?}", solve_p1(&i));
    println!("P1 e: {:?}", solve_p2(&e));
    println!("P1 i: {:?}", solve_p2(&i));
}
