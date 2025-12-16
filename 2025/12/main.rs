use std::collections::HashSet;
use std::collections::VecDeque;

#[derive(Debug)]
enum E {
    Input(&'static str, std::io::Error),
    Parse(&'static str),
    ParseNum(&'static str, String),
}

fn parse_unum(i: &str) -> Result<usize, E> {
    i.parse::<usize>().map_err(|e| E::ParseNum("not a unumber", i.to_string()))
}

type Coord = (usize, usize);
type Shape = Vec<Coord>;

#[derive(Debug)]
struct Problem {
    region: (usize, usize),
    inventory: Vec<usize>,
}

#[derive(Debug)]
struct Input {
    shapes: Vec<Shape>,
    problems: Vec<Problem>,
}

fn parse_shape(i: &str) -> Result<Shape, E> {
    let mut shape: Shape = Shape::new();

    for (y, l) in i.lines().skip(1).enumerate() {
        for (x, c) in l.chars().enumerate() {
            match c {
                '.' => continue,
                '#' => { shape.push((x, y)); },
                _   => return Err(E::Parse("unexpected char in the shape")),
            }
        }
    }

    shape.sort();
    Ok(shape)
}

fn parse_problem(i: &str) -> Result<Problem, E> {
    let Some((rs, ss)) = i.split_once(": ") else {
        return Err(E::Parse("no problem separator"))
    };
    let Some((xrs, yrs)) = rs.split_once('x') else {
        return Err(E::Parse("no region separator"))
    };
    let region = (parse_unum(xrs)?, parse_unum(yrs)?);
    let inventory: Vec<usize> =
        ss.split(' ')
          .map(|s| parse_unum(s))
          .collect::<Result<_, _>>()?;

    Ok(Problem{
        region,
        inventory,
    })
}

fn parse_input(i: &str) -> Result<Input, E> {
    let mut bv: Vec<&str> = i.split("\n\n").collect();

    let Some(ps) = bv.pop() else { return Err(E::Parse("not enough input blocks")) };

    let shapes: Vec<Shape> = bv.into_iter().map(|b| parse_shape(b)).collect::<Result<_, _>>()?;
    let problems: Vec<Problem> = ps.lines().map(|l| parse_problem(l)).collect::<Result<_,_>>()?;

    Ok(Input{
        shapes,
        problems,
    })
}

// flip along y axis:
// x -> -x (+max_x)
// y -> y
fn flip_horiz(cs: &[Coord]) -> Vec<Coord> {
    let max_x: usize = cs.iter().map(|e| e.0).max().expect("at least one point");

    cs.iter().map(|(x,y)| (max_x - *x, *y)).collect()
}

// rotate 90 degrees clockwise:
// x -> -y (+ max_y)
// y -> x
fn rota(cs: &[Coord]) -> Vec<Coord> {
    let max_x: usize = cs.iter().map(|e| e.0).max().expect("at least one point");
    let max_y: usize = cs.iter().map(|e| e.1).max().expect("at least one point");

    let mut r: Vec<Coord> = cs.iter().map(|(x,y)| (max_y - *y, *x)).collect();

    r
}

fn rotas_flips(s: &Shape) -> Vec<Shape> {
    // to dedupte
    let mut seen = HashSet::<Vec<Coord>>::new();

    let mut cs: Vec<Coord> = s.iter().cloned().collect();
    for _ in 0..4 {
        cs = rota(&cs);
        seen.insert(cs.clone());
        seen.insert(flip_horiz(&cs));
    }

    seen.into_iter().map(|v| v.into_iter().collect()).collect()
}

fn solve_p1_one(p: &Problem, shapes: &Vec<Shape>) -> bool {
    // Some high-level hints:
    // track usable boundary and attach to it directly
    // TODO: memoize boundary result (maybe?)

    let sq = p.region.0 * p.region.1;
    let takes = p.inventory.iter().enumerate().map(|(i, n)|
        n * shapes[i].len()
    ).sum::<usize>();

    // Do we even have enough space?
    if takes > sq { return false }

    // build rotated and flipped shapes
    let rshapes: Vec<Vec<Shape>> = shapes.iter().map(|s| rotas_flips(s)).collect();

    // Run a search

    #[derive(Clone)]
    struct Field {
        taken: HashSet<Coord>,
    }

    impl Field {
        fn new() -> Field { Field{ taken: HashSet::new() } }
        fn contains(self: &Self, c: &Coord) -> bool { self.taken.contains(c) }
        fn insert(self: &mut Self, c: Coord) { self.taken.insert(c); }
    }

    #[derive(Clone)]
    struct State {
        field: Field,
        inventory: Vec<usize>, // not yet used shape counts
    }
    let mut q = VecDeque::<State>::new();
    q.push_back(State{
        field: Field::new(),
        inventory: p.inventory.clone(),
    });

    let mut seen = HashSet::<(Vec<Coord>, Vec<usize>)>::new();

    'next_state: while let Some(s) = q.pop_back() {
        // managed to arrange all the inventory
        if s.inventory.iter().all(|e| *e == 0) { return true }

        // skip visited
        let mut field_coords: Vec<Coord> = s.field.taken.iter().cloned().collect();
        field_coords.sort();
        let seen_key = (field_coords, s.inventory.clone());
        if seen.contains(&seen_key) { continue }
        seen.insert(seen_key);

        let will_take = s.inventory.iter().enumerate().map(|(i, n)|
            n * shapes[i].len()
        ).sum::<usize>();

        // otherwise try to place all the puzzle pieces
        for (ix, count) in s.inventory.iter().enumerate() {
            if *count == 0 { continue }

            for shape in &rshapes[ix] {
                let mut empty_cells: usize = 0;
                for x in 0..p.region.0 {
                    'next_point: for y in 0..p.region.1 {
                        if sq < s.field.taken.len() + will_take + empty_cells { continue 'next_state }
                        if s.field.contains(&(x, y)) { empty_cells += 1; }
                        // check shape for collision
                        for (dx, dy) in shape {
                            let sc = (x + dx, y + dy);
                            if sc.0 >= p.region.0 { continue 'next_point }
                            if sc.1 >= p.region.1 { continue 'next_point }
                            if s.field.contains(&sc) { continue 'next_point }
                        }

                        // Shape fits! Trying.
                        let mut new_state = s.clone();
                        for (dx, dy) in shape {
                            let sc = (x + dx, y + dy);
                           new_state.field.insert(sc);
                        }
                        new_state.inventory[ix] -= 1;

                        q.push_back(new_state);
                    }
                }
            }
        }
    }

    false
}

fn solve_p1(i: &str) -> Result<usize, E> {
    let ip = parse_input(i)?;

    Ok(ip.problems
         .iter()
         .filter(|p| solve_p1_one(p, &ip.shapes))
         .count())
}

fn main() -> Result<(), E> {
    let e = std::fs::read_to_string("example")
        .map_err(|e| E::Input("example", e))?;
    let i = std::fs::read_to_string("input")
        .map_err(|e| E::Input("inmput", e))?;

    eprintln!("===---               ---===");
    eprintln!("WARINING: The solution takes 50GiB and 30 minutes of run time. You have been warned.");
    eprintln!("===---               ---===");
    eprintln!("P1 e: {:?}", solve_p1(&e)?);
    eprintln!("P1 e: {:?}", solve_p1(&i)?);

    Ok(())
}
