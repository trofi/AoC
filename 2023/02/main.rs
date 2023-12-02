struct Round {
    r: usize,
    g: usize,
    b: usize,
}

struct Game {
    ix: usize,
    rounds: Vec<Round>,
}

fn parse_round(i: &str) -> Round {
    let mut r = 0usize;
    let mut g = 0usize;
    let mut b = 0usize;

    // Example: " 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green"
    for e in i.split(",") {
        let v: Vec<&str> = e.split(" ").collect();
        match v.as_slice() {
            ["", n, "red"] => { r = n.parse().expect("a number"); },
            ["", n, "green"] => { g = n.parse().expect("a number"); },
            ["", n, "blue"] => { b = n.parse().expect("a number"); },
            _ => panic!("'{}' / '{:?}' is ont recognized yet", e, v),
        }
    }

    Round{r,g,b}
}

fn parse_game(i: &str) -> Game {
    // Example: "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green"
    let by_col: Vec<&str> = i.split(":").collect();
    assert!(by_col.len() == 2);

    let ix: usize = by_col[0]["Game ".len()..].parse().expect("a number");
    let rv: Vec<&str> = by_col[1].split(";").collect();
    let rounds: Vec<Round> = rv.into_iter().map(|l| parse_round(l)).collect();

    Game{ix,rounds}
}

fn solve_p1(i: &str, bag: &Round) -> usize {
    let games: Vec<Game> = i.lines().map(|l| parse_game(l)).collect();

    games.into_iter()
         .filter(|g| g.rounds.iter().all(|r| r.r <= bag.r && r.g <= bag.g && r.b <= bag.b))
         .map(|g| g.ix)
         .sum()
}

fn main() {
    let e = std::fs::read_to_string("example").expect("example");
    let i = std::fs::read_to_string("input").expect("input");
    println!("P1 example: {}", solve_p1(&e, &Round{r: 12, g: 13, b: 14}));
    println!("P1 input:   {}", solve_p1(&i, &Round{r: 12, g: 13, b: 14}));
}
