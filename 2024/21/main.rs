use std::collections::BTreeSet;
use std::collections::HashMap;

fn parse_input(i: &str) -> Vec<(Vec<char>, usize)> {
    i.lines()
     .map(|l| (l.chars().collect(), l[0..l.len()-1].parse::<usize>().expect("a number")))
     .collect()
}

#[derive(Clone, Eq, Hash, Ord, PartialOrd, PartialEq)]
struct State<'a> {
    to_output: &'a [char],
    n: char, // numeric
    dv: Vec<char>, // directional (d[0] is user-facing, d[len-1] is numeric-facing
}

fn mk_initial(i: &[char], depth: usize) -> State<'_> {
    let mut dv: Vec<char> = Vec::new();
    dv.resize(depth, 'A');

    State {
        to_output: i,
        n: 'A',
        dv,
    }
}

fn handle_move<'a>(s: &State<'a>) -> Vec<State<'a>> {
    let mut r: Vec<State> = Vec::new();

    // move d[0] itself one available direction around:
    //   ^ A
    // < v >
    let neighbors: &[char] = match s.dv[0] {
        '^' => &['A', 'v'],
        'A' => &['^', '>'],
        '<' => &['v'],
        'v' => &['^', '<', '>'],
        '>' => &['A', 'v'],
        _ => panic!("Not yet handled dv[0]={:?} move", s.dv[0]),
    };

    for ne in neighbors {
        let mut dv = s.dv.clone();
        dv[0] = *ne;

        r.push(State{
            to_output: s.to_output,
            n: s.n,
            dv,
        });
    }

    r
}

fn handle_push<'a>(mut r: State<'a>) -> Option<State<'a>> {
    //let mut r = s.clone();

    // directional -> directional: we handle the push
    for ix in 1..r.dv.len() {
        // push dv[ix-1] and affect dv[ix] state
        //     ^ A
        //   < v >
        let od = match (r.dv[ix-1], r.dv[ix]) {
            // pushing the target
            ('A', _) => continue,

            // positioning:
            // '<'
            ('<', '^') => None,
            ('<', 'A') => Some('^'),
            ('<', '<') => None,
            ('<', 'v') => Some('<'),
            ('<', '>') => Some('v'),
            // '>'
            ('>', '^') => Some('A'),
            ('>', 'A') => None,
            ('>', '<') => Some('v'),
            ('>', 'v') => Some('>'),
            ('>', '>') => None,
            // '^'
            ('^', '^') => None,
            ('^', 'A') => None,
            ('^', '<') => None,
            ('^', 'v') => Some('^'),
            ('^', '>') => Some('A'),
            // 'v'
            ('v', '^') => Some('v'),
            ('v', 'A') => Some('>'),
            ('v', '<') => None,
            ('v', 'v') => None,
            ('v', '>') => None,

            (dp, d) => panic!("Unhandled dv[{}/{}]: {}/{}", ix-1, ix, dp, d),
        };

        if let Some(d) = od {
            r.dv[ix] = d;
            return Some(r);
        }

        return None;
    }

    // directional -> numeric

    // push
    //   dv[len-1]:
    //     ^ A
    //   < v >
    //
    //   n:
    //   7 8 9
    //   4 5 6
    //   1 2 3
    //     0 A
    let ix = r.dv.len() - 1;

    let on = match (r.dv[ix], r.n) {
        // push numeric
        ('A', n) => {
            if n == r.to_output[0] {
                // pressed expected button, no moves around
                r.to_output = &r.to_output[1..];
                return Some(r);
            };
            None
        },
        // position numeric
        // '<'
        ('<', '7') => None,
        ('<', '8') => Some('7'),
        ('<', '9') => Some('8'),
        ('<', '4') => None,
        ('<', '5') => Some('4'),
        ('<', '6') => Some('5'),
        ('<', '1') => None,
        ('<', '2') => Some('1'),
        ('<', '3') => Some('2'),
        ('<', '0') => None,
        ('<', 'A') => Some('0'),
        // '>'
        ('>', '7') => Some('8'),
        ('>', '8') => Some('9'),
        ('>', '9') => None,
        ('>', '4') => Some('5'),
        ('>', '5') => Some('6'),
        ('>', '6') => None,
        ('>', '1') => Some('2'),
        ('>', '2') => Some('3'),
        ('>', '3') => None,
        ('>', '0') => Some('A'),
        ('>', 'A') => None,
        // '^'
        ('^', '7') => None,
        ('^', '8') => None,
        ('^', '9') => None,
        ('^', '4') => Some('7'),
        ('^', '5') => Some('8'),
        ('^', '6') => Some('9'),
        ('^', '1') => Some('4'),
        ('^', '2') => Some('5'),
        ('^', '3') => Some('6'),
        ('^', '0') => Some('2'),
        ('^', 'A') => Some('3'),
        // 'v'
        ('v', '7') => Some('4'),
        ('v', '8') => Some('5'),
        ('v', '9') => Some('6'),
        ('v', '4') => Some('1'),
        ('v', '5') => Some('2'),
        ('v', '6') => Some('3'),
        ('v', '1') => None,
        ('v', '2') => Some('0'),
        ('v', '3') => Some('A'),
        ('v', '0') => None,
        ('v', 'A') => None,

        (d, n)     => panic!("Unhandled d={:?} n={:?}", d, n),
    };

    if let Some(n) = on {
        r.n = n;
        return Some(r)
    }
    return None
}

fn solve_one(i: &[char], depth: usize) -> usize {
    let mut r = usize::MAX;

    #[derive(Eq, Ord, PartialOrd, PartialEq)]
    struct Entry<'a> {
        prio: isize,
        steps: usize,
        key: State<'a>,
    }

    fn mk_entry<'a>(steps: usize, key: State<'a>) -> Entry<'a> {
        let prio = (steps as isize) + (key.to_output.len() as isize);
        Entry {
            prio,
            steps,
            key,
        }
    }

    let mut queue: BTreeSet<Entry> = BTreeSet::new();
    queue.insert(mk_entry(0, mk_initial(i, depth)));

    let mut visited: HashMap<State, usize> = HashMap::new();

    while let Some(Entry{prio: _, key, steps}) = queue.pop_first() {
        if key.to_output.len() == 0 {
            if r > steps {
                r = steps;
            }
            continue
        }

        if steps >= r { continue }

        if let Some(seen_steps) = visited.get(&key) {
            // already explored a better solution
            if steps >= *seen_steps { continue }
        }
        visited.insert(key.clone(), steps);

        for s in handle_move(&key) {
            queue.insert(mk_entry(steps + 1, s));
        }

        if let Some(s) = handle_push(key) {
            queue.insert(mk_entry(steps + 1, s));
        }
    }

    r
}

fn solve(i: &str, depth: usize) -> usize {
    let cs = parse_input(i);

    cs.into_iter()
      .map(|c| solve_one(&c.0, depth) * c.1)
      .sum()
}

fn main() {
    let e = std::fs::read_to_string("example").expect("example");
    let i = std::fs::read_to_string("input").expect("input");
    println!("P1 e: {:?} (expect 126384)", solve(&e, 2));
    println!("P1 i: {:?}", solve(&i, 2));
    // will not work for depth if 25, it's answer will likely be
    // too many digits to traverse even once :)
    //for ix in 3..=25 {
        //println!("P2 i {:?} (depth={ix:?})", solve(&i, ix));
    //}
}
