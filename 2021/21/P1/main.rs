use std::{*, collections::*, fs::*, io::*, iter::*};

struct State {
    state: usize,
    score: usize,
}

fn get_input(input_file: &str) -> Vec<State> {
    let mut reader = BufReader::new(File::open(input_file).unwrap());

    return Vec::from_iter(
        reader.lines().enumerate().map(|(i,ol)|{
            // "Player 1 starting position: 4"
            let l = ol.expect("no errors");
            let ls = Vec::from_iter(l.split(' '));
            assert!(1 + i == ls[1].parse::<usize>().expect("number"));

            State {
                state: ls[4].parse::<usize>().expect("number"),
                score: 0,
            }
        })
    )
}

struct DieState {
    rolls: usize,
    state: usize,
}

fn roll (d: &mut DieState) -> usize {
    let s = d.state;
    d.rolls += 1;
    d.state = (d.state - 1 + 1) % 100 + 1;
    return s;
}

fn main() {
    for input_file in ["example", "input"] {
        let input = get_input(input_file);

        let mut i = 0;

        let mut die = DieState{
            rolls: 0,
            state: 1,
        };
        let mut players = input;

        loop {
            let mut v = roll(&mut die); v += roll(&mut die); v += roll(&mut die);
            players[i].state = (players[i].state - 1 + v) % 10 + 1;
            players[i].score += players[i].state;
            //println!("p[{}] -> {}, score={} v={}", i + 1, players[i].state, players[i].score, v);
            if players[i].score >= 1000 { break; }

            i = (i + 1) % 2;
        }
        let loser = (i + 1) % 2;

        let ans = die.rolls * players[loser].score;

        println!("{}: {} = {} * {}", input_file, ans, die.rolls, players[loser].score);
    }
}
