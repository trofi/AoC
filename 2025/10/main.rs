use std::collections::BTreeSet;
use std::collections::BTreeMap;
use std::collections::HashSet;
use std::collections::HashMap;
use std::collections::VecDeque;

#[derive(Debug)]
enum E {
    Input(&'static str, std::io::Error),
    Parse(&'static str, String),
    Eval(&'static str),
}

#[derive(Debug)]
struct Desc {
    lights: usize,
    wirings: Vec<Vec<usize>>,
    joltage: Vec<usize>,
}

fn parse_lights(i: &str) -> Result<usize, E> {
    let mut r = 0;
    for c in i.chars().rev() {
        let b = match c {
            '#' => 1,
            '.' => 0,
            _   => return Err(E::Parse("Unespected light", c.to_string())),
        };
        r = r * 2 + b; 
    }

    Ok(r)
}

fn parse_num(i: &str) -> Result<usize, E> {
    i.parse::<usize>()
     .map_err(|e| E::Parse("not a number", e.to_string()))
}

fn parse_num_seq(i: &str) -> Result<Vec<usize>, E> {
    i.split(',')
     .map(|e| parse_num(e))
     .collect()
}

fn parse_desc(i: &str) -> Result<Desc, E> {
    // strip markup, leave only parameter ordering
    let mut wv: VecDeque<&str> =
        i.split(['[', ']', ' ', '(', ')', '{', '}'])
         .filter(|e| e.len() > 0)
         .collect();

    let lights = parse_lights(
        wv.pop_front()
          .ok_or_else(|| E::Parse("not enough parts for lights", i.to_string()))?)?;
    let joltage = parse_num_seq(wv.pop_back()
          .ok_or_else(|| E::Parse("not enough parts for wirings", i.to_string()))?)?;
    let wirings: Vec<Vec<usize>> = wv
          .into_iter()
          .map(|v| parse_num_seq(v)).collect::<Result<_, _>>()?;

    Ok(Desc{
        lights,
        wirings,
        joltage,
    })
}

fn parse_input(i: &str) -> Result<Vec<Desc>, E> {
    i.lines().map(|l| parse_desc(l)).collect()
}

fn solve_p1_one(d: &Desc) -> Result<usize, E> {
    // collect to bitvec
    let wv: Vec<usize> =
        d.wirings
     .iter()
         .map(|w| w.iter().map(|e| 1 << e).sum())
         .collect();
    // a queue of (lights_state, depth)
    let mut q: VecDeque<(usize, usize)> = VecDeque::new();
    let mut visited: HashSet<usize> = HashSet::new();
    q.push_back((0, 0));

    while let Some((ls, n)) = q.pop_front() {
        if visited.contains(&ls) { continue }
        visited.insert(ls);

        // found the solution
        if ls == d.lights { return Ok(n) }

        // search further
        for w in &wv { q.push_back((ls ^ *w, n + 1)); }
    }

    Err(E::Eval("no solution"))
}

fn solve_p1(i: &str) -> Result<usize, E> {
    let ds = parse_input(i)?;
    ds.into_iter().map(|d| solve_p1_one(&d)).sum()
}

fn solve_p2_one(d: &Desc) -> Result<isize, E> {
    // we need to solve the following problem:
    //
    // a1 * button1 + a2 * button2 + ... aN * buttonN = joltage
    // and we need to find a min(a1 + .... + aN);
    //
    // It's a system of simple linear equations and a linear function
    // to minimize. Sometimes there is a single solution (and nothing to
    // minimize), easy case. But sometimes is a linear function with
    // multiple parameters.

    // solve system of linear euqations (matrix form):
    // A x WS = JOLTAGE

    // build wirings as vector rows
    let mut ws: Vec<Vec<isize>> = Vec::new();
    for w in &d.wirings {
        let mut v: Vec<isize> = Vec::new();
        v.resize(d.joltage.len(), 0);
        for i in w {
            v[*i] = 1;
        }
        ws.push(v);
    }

    // build A x WS = J
    let mut j: Vec<isize> = d.joltage.iter().map(|e| *e as isize).collect();
    let mut aws: Vec<Vec<isize>> = (0..j.len()).map(|i|
        ws.iter()
          .map(|w| w[i])
          .collect()
    ).collect();

    //eprintln!("d={d:?}");
    //for (r, aw) in aws.iter().enumerate() { eprintln!("aw={aw:?} | {:?}", j[r]); }

    // simplify using gaussian-like pass: top-down, then bottom-up
    for r in (0..aws.len()).chain((0..aws.len()).rev()) {
        // find non-zero column in the row
        let c: usize = match aws[r].iter().position(|e| *e != 0) {
            None => continue,
            Some(p) => p,
        };

        // eliminate chosen column's data
        for er in 0..aws.len() {
            if r == er { continue }
            if aws[er][c] == 0 { continue }

            // value to multiply 'r' for before addition
            let mr = -aws[er][c];
            // value to multiply 'er' for before addition
            let mer = aws[r][c];

            let new_er: Vec<isize> =
                aws[r].iter().zip(aws[er].iter())
                      .map(|(v, ev)| *v * mr + *ev * mer)
                      .collect();

            aws[er] = new_er;
            j[er] = j[r] * mr + j[er] * mer;
        }
    }

    // There are sometimes multiple solutions. Pick shortest by trying
    // a range of free variables. We can get up to 3 free variables.
    // As they define "wight" of the button let's just enumerate through
    // all of them.

    // TODO: very inefficient!

    // Let's track possible solutions as vector of sets.
    let max_v: isize = *d.joltage.iter().max().ok_or_else(|| E::Eval("no values in joltage"))? as isize;

    // Variable index to variable value. If the variable is unset then
    // it can take all values from 0 to max_v.
    type Solution = Vec<Option<isize>>;
    let mut solutions: Vec<Solution> = Vec::new();
    let mut ini: Vec<Option<isize>> = Vec::new();
    ini.resize(d.wirings.len(), None);
    solutions.push(ini);

    // Refine the solution:
    // remove solutions that don't match
    // extend solution with new data
    fn expand_solution(s: Solution, row: &[isize], jv: isize, known_bits: isize) -> Vec<Solution> {
        let mut known_bits = known_bits;
        let mut r = Vec::new();

        let mut row_sum = 0;
        let mut did_expand: bool = false;
        for ix in 0..row.len() {
            if row[ix] == 0 { continue }

            match s[ix] {
                // known variable: substitute
                Some(v) => {
                    row_sum += v * row[ix];
                    known_bits -= v;
                },
                // unknown variable: expand
                None => {
                    did_expand = true;
                    for v in 0..=known_bits {
                        let mut ns = s.clone();
                        ns[ix] = Some(v);

                        for es in expand_solution(ns, row, jv, known_bits - v).into_iter() {
                            r.push(es);
                        }
                    }
                }
            };
        }
        // the solution did not contradict the equation 's * row = jv'
        if row_sum == jv && !did_expand {
            r.push(s);
        }

        r
    }

    // scan through each row, generate all possible solution ranges
    // and match against the rest of rows.
    for r in 0..aws.len() {
        if aws[r].iter().all(|e| *e == 0) { continue }

        let mut new_solutions = Vec::new();
        for s in solutions.into_iter() {
            for ns in expand_solution(s, &aws[r], j[r], max_v).into_iter() {
                new_solutions.push(ns);
            }
        }
        solutions = new_solutions;
    }

    Ok(solutions
        .iter()
        .map(|s| s.iter().map(|e| e.expect("has a solution")).sum::<isize>())
        .min()
        .ok_or_else(|| E::Eval("No solutions"))?)
}

fn solve_p2(i: &str) -> Result<isize, E> {
    let ds = parse_input(i)?;
    ds.into_iter().map(|d| solve_p2_one(&d)).sum()
}

fn main() -> Result<(), E> {
    let e = std::fs::read_to_string("example")
        .map_err(|e| E::Input("example", e))?;
    let i = std::fs::read_to_string("input")
        .map_err(|e| E::Input("input", e))?;

    println!("P1 e: {:?}", solve_p1(&e)?);
    println!("P1 e: {:?}", solve_p1(&i)?);

    println!("P2 e: {:?}", solve_p2(&e)?);
    println!("P2 i: {:?}", solve_p2(&i)?);

    Ok(())
}
