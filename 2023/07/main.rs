use std::cmp::Ordering;
use std::collections::HashMap;

#[derive(Eq, PartialEq, Ord)]
struct Hand<'a>{
    h: &'a str,
    v: usize,
    jokers: bool,
}

#[derive(PartialEq, Eq, PartialOrd, Ord)]
enum Type {
    High,
    Pair,
    TwoPairs,
    Three,
    House,
    Four,
    Five,
}

#[derive(PartialEq, Eq, PartialOrd, Ord)]
struct Value {
    t: Type,
    r: Vec<usize>,
}

const RANKS: &str       = "23456789TJQKA";
const JOKER_RANKS: &str = "J23456789TQKA";

impl Hand<'_> {
    fn to_joker(self: &Self) -> Self {
        Self {
            h: self.h,
            v: self.v,
            jokers: true,
        }
    }

    fn hand_type(self: &Self) -> Type {
        let mut hist: HashMap<char, usize> = HashMap::new();
        for c in self.h.chars() {
            hist.entry(c)
                .and_modify(|e| *e +=1 )
                .or_insert(1);
        }

        let jokers = if !self.jokers {
            0
        } else {
            *hist.remove(&'J').get_or_insert(0)
        };

        let mut v: Vec<usize> = hist.values().cloned().collect();
        v.sort();

        let l = v.len();
        if l > 0 {
            v[l - 1] += jokers;
        } else {
            v.push(jokers);
        }

        match v.as_slice() {
            [1, 1, 1, 1, 1] => Type::High,
            [1, 1, 1, 2]    => Type::Pair,
            [1, 2, 2]       => Type::TwoPairs,
            [1, 1, 3]       => Type::Three,
            [2, 3]          => Type::House,
            [1, 4]          => Type::Four,
            [5]             => Type::Five,
            _ => panic!("Unhandled {:?} hand", v),
        }
    }
    fn value(self: &Self) -> Value {
        Value{
            t: self.hand_type(),
            r: self.h.chars().map(|c|
                if self.jokers {
                    JOKER_RANKS.find(c).expect("present")
                } else {
                    RANKS.find(c).expect("present")
                }
               ).collect()
        }
    }
}

impl PartialOrd for Hand<'_> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.value().cmp(&other.value()))
    }
}

fn parse_hand(i: &str) -> Hand {
    let iv: Vec<&str> = i.split(" ").collect();
    match iv.as_slice() {
        [h, sv] => Hand{
            h,
            v: sv.parse().expect("a number"),
            jokers: false,
        },
        _ => panic!("Unhandled {:?} card", iv),
    }
}

fn parse_input(i: &str) -> Vec<Hand> {
    i.lines().map(|l| parse_hand(l)).collect()
}

fn solve_p1(i: &str) -> usize {
    let mut i = parse_input(i);

    i.sort();

    i.into_iter()
     .enumerate()
     .map(|(ix, c)| (ix + 1) * c.v)
     .sum()
}

fn solve_p2(i: &str) -> usize {
    let mut i: Vec<Hand> =
        parse_input(i).into_iter()
                      .map(|e| e.to_joker())
                      .collect();

    i.sort();

    i.into_iter()
     .enumerate()
     .map(|(ix, c)| (ix + 1) * c.v)
     .sum()
}

fn main() {
    let e = std::fs::read_to_string("example").expect("example");
    let i = std::fs::read_to_string("input").expect("input");
    println!("P1 example: {}", solve_p1(&e));
    println!("P1   input: {}", solve_p1(&i));
    println!("P2 example: {}", solve_p2(&e));
    println!("P2   input: {}", solve_p2(&i));
}
