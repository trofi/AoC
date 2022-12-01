use std::{*, collections::*, fs::*, io::*, iter::*};

#[derive(Debug,PartialEq,Eq,Hash,Clone)]
struct Player {
    state: usize,
    score: usize,
}

fn get_input(input_file: &str) -> Vec<Player> {
    let reader = BufReader::new(File::open(input_file).unwrap());

    return Vec::from_iter(
        reader.lines().enumerate().map(|(i,ol)|{
            // "Player 1 starting position: 4"
            let l = ol.expect("no errors");
            let ls = Vec::from_iter(l.split(' '));
            assert!(1 + i == ls[1].parse::<usize>().expect("number"));

            Player {
                state: ls[4].parse::<usize>().expect("number"),
                score: 0,
            }
        })
    )
}

#[derive(Debug,PartialEq,Eq,Hash)]
struct GameState {
    next_p: usize,
    players: Vec<Player>,
}

#[derive(Debug)]
struct AllStates {
    ongoing: HashMap<GameState, usize>,
    finished: HashMap<usize, usize>,
}

fn step(s: &AllStates, rolls: &HashMap::<usize, usize>) -> AllStates {
    let mut r = AllStates{
        ongoing: HashMap::new(),
        finished: s.finished.clone(),
    };

    for (d, dc) in rolls.iter() {
       for (gs, gc) in s.ongoing.iter() {
           let i = gs.next_p;
           let mut ngs = GameState{
               next_p: (i + 1) % 2,
               players: gs.players.clone(),
           };
           let pl = &mut ngs.players[i];
           pl.state = (pl.state - 1 + d) % 10 + 1;
           pl.score += pl.state;

           if pl.score >= 21 {
               *r.finished.entry(i).or_insert(0) += dc * gc;
           } else {
               *r.ongoing.entry(ngs).or_insert(0) += dc * gc;
           }
       }
    }

    return r;
}

fn main() {
    let mut r = HashMap::<usize, usize>::new();
    r.insert(0,1);

    for _ in 1..=3 {
        let mut nr = HashMap::new();
        for d in 1..=3 {
            for (v, c) in r.iter() {
                *nr.entry(d + v).or_insert(0) += *c;
            }
        }
        r = nr;
    }

    for input_file in ["example", "input"] {
        let input = get_input(input_file);

        let mut s = AllStates{
            ongoing: HashMap::new(),
            finished: HashMap::new(),
        };
        s.ongoing.insert(GameState{
                next_p
                : 0,
                players: input,
        }, 1);

        while s.ongoing.len() != 0 {
            s = step(&s, &r);
        }

        println!("{}: {:?}", input_file, s.finished.values().max().unwrap()
        );
    }
}
