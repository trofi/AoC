use std::collections::HashMap;

type Coord = (isize, isize);
type Map = HashMap<Coord, char>;

struct Input {
    pos: Coord,
    map: Map,
    moves: Vec<char>,
}

fn parse_input(i: &str) -> Input {
    let (map_s, moves_s) = i.split_once("\n\n").expect("map and moves");

    let mut map = Map::new();
    let mut pos: Option<Coord> = None;

    for (y, l) in map_s.lines().enumerate() {
        for (x, c) in l.chars().enumerate() {
            match c {
                '.'       => {},
                '@' | '#' | 'O' => {
                    map.insert((x as isize, y as isize), c);
                    if c == '@' {
                        pos = Some((x as isize, y as isize));
                    }
                },
                _         => panic!("Unhandled {:?} cell", c),
            }
        }
    }

    let mut moves: Vec<char> = Vec::new();
    for l in moves_s.lines() {
        for c in l.chars() {
            moves.push(c);
        }
    }

    Input{
        pos: pos.expect("an initial position"),
        map,
        moves,
    }
}

fn csum(m: &Map) -> isize {
    let mut r = 0;

    for (pos, c) in m {
        if *c == 'O' || *c == '[' {
            r += pos.0 + 100 * pos.1;
        }
    }

    r
}

fn advance(p: &Coord, c: char) -> Coord {
    let d = match c {
        '>' => ( 1,  0),
        '<' => (-1,  0),
        '^' => ( 0, -1),
        'v' => ( 0,  1),
        _   => panic!("Inhandned {:?} move", c),
    };

    (p.0 + d.0, p.1 + d.1)
}

fn get_connected(pos: &Coord, c: char, m: &Map) -> Map {
    let mut r = Map::new();

    let mut queue: Vec<Coord> = Vec::new();
    queue.push(*pos);

    while let Some(p) = queue.pop() {
        if let Some(pc) = m.get(&p) {
            if !m.contains_key(&p) { continue }
            if *pc == '#' { continue }
            if r.contains_key(&p) { continue }

            // All non-# elements are movable individually.
            r.insert(p, *pc);

            // Enqueue next neighbor.
            queue.push(advance(&p, c));

            // For vertical moves consider the rest of the box.
            if c == 'v' || c == '^' {
                match pc {
                    ']' => {
                        // also include left-of
                        let p = (p.0 - 1, p.1);
                        assert!(m.get(&p) == Some(&'['));
                        queue.push(p);
                    },
                    '[' => {
                        // handle right-of
                        let p = (p.0 + 1, p.1);
                        assert!(m.get(&p) == Some(&']'));
                        queue.push(p);
                    },
                    '@' | 'O' => {},
                    _ => panic!("Unhandled {:?} neighbour type", pc),
                }
            }
        }
    }

    r
}

fn can_move(a: &Map, c: char, m: &Map) -> bool {
    for (p, _) in a {
        let ap = advance(p, c);
        if a.contains_key(&ap) { continue }
        if m.contains_key(&ap) { return false }
    }

    true
}

fn do_move(a: &Map, c: char, m: &mut Map) {
    for (p, _) in a { m.remove(p); }
    for (p, v) in a { m.insert(advance(p, c), *v); }
}

fn solve_p1(i: &str) -> isize {
    let Input{mut pos, mut map, moves} = parse_input(i);

    for c in moves.into_iter() {
        let a = get_connected(&pos, c, &map);
        let new_pos = advance(&pos, c);

        if !can_move(&a, c, &map) { continue }
        pos = new_pos;
        do_move(&a, c, &mut map);
    }


    csum(&map)
}

fn expand_for_p2(m: &Map) -> Map {
    let mut r = Map::new();

    for (p, c) in m {
        let epl = (p.0 * 2 + 0, p.1);
        let epr = (p.0 * 2 + 1, p.1);
        match c {
            '#' => {
                r.insert(epl, '#');
                r.insert(epr, '#');
            }
            'O' => {
                r.insert(epl, '[');
                r.insert(epr, ']');
            }
            '@' => {
                r.insert(epl, '@');
            }
            _ => panic!("Unhandled {:?} in expansion", c),
        }
    }

    r
}

fn solve_p2(i: &str) -> isize {
    let Input{mut pos, mut map, moves} = parse_input(i);

    pos.0 *= 2;
    map = expand_for_p2(&map);

    for c in moves.into_iter() {
        let a = get_connected(&pos, c, &map);
        let new_pos = advance(&pos, c);

        if !can_move(&a, c, &map) { continue }
        pos = new_pos;
        do_move(&a, c, &mut map);
    }

    csum(&map)
}

fn main() {
    let e = std::fs::read_to_string("example").expect("example");
    let e2 = std::fs::read_to_string("example2").expect("example2");
    let i = std::fs::read_to_string("input").expect("input");
    println!("P1  e: {:?}", solve_p1(&e));
    println!("P1 e2: {:?}", solve_p1(&e2));
    println!("P1  i: {:?}", solve_p1(&i));
    println!("P2 e2: {:?}", solve_p2(&e2));
    println!("P2  i: {:?}", solve_p2(&i));
}
