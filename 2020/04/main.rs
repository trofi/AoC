use std::collections::HashMap;
use std::collections::HashSet;
use std::iter::FromIterator;

type Entry<'a> = HashMap<&'a str, &'a str>;

fn parse_entry<'a>(i: &'a str) -> Entry<'a> {
    i.trim()
     .split(&[' ', '\n'])
     .map(|e| {
         e.split_once(':')
          .expect("a pair")
     })
     .collect()
}

fn parse_input<'a>(i: &'a str) -> Vec<Entry<'a>> {
    i.split("\n\n").map(|l| parse_entry(l)).collect()
}

fn solve_p1(i: &str) -> usize {
    let ps = parse_input(i);
    let expected: HashSet<&str> = HashSet::from([
        "byr",
        "iyr",
        "eyr",
        "hgt",
        "hcl",
        "ecl",
        "pid",
        //"cid",
    ]);

    ps.iter().filter(|p| {
        let actual: HashSet<&str> = HashSet::from_iter(p.keys().cloned());

        expected.is_subset(&actual)
    }).count()
}

fn solve_p2(i: &str) -> usize {
    let ps = parse_input(i);
    let expected: HashSet<&str> = HashSet::from([
        "byr",
        "iyr",
        "eyr",
        "hgt",
        "hcl",
        "ecl",
        "pid",
        //"cid",
    ]);

    ps.iter().filter(|p| {
        let actual: HashSet<&str> = HashSet::from_iter(p.keys().cloned());

        expected.is_subset(&actual) &&
        p.into_iter().all(|e| {
            match e {
                // ideally should be cached or even range-checked
                (&"byr", s) => (1920..=2002).map(|e| format!("{}", e)).collect::<Vec<_>>().contains(&s.to_string()),
                (&"iyr", s) => (2010..=2020).map(|e| format!("{}", e)).collect::<Vec<_>>().contains(&s.to_string()),
                (&"eyr", s) => (2020..=2030).map(|e| format!("{}", e)).collect::<Vec<_>>().contains(&s.to_string()),
                (&"hgt", s) => (150..=193).map(|e| format!("{}cm", e)).collect::<Vec<_>>().contains(&s.to_string()) ||
                               (59..=76).map(|e| format!("{}in", e)).collect::<Vec<_>>().contains(&s.to_string()),

                (&"pid", s) => s.len() == 9 && s.chars().all(|c| c.is_digit(10)),
                (&"hcl", s) => s.len() == 7 && &s[0..1] == "#" && s[1..].chars().all(|c| "0123456789abcdef".contains(c)),

                (&"ecl", s) => ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"].contains(s),
                (&"cid", _) => true,
                _ => panic!("Unhandled {:?} record", e),
            }
        })
    }).count()
}

fn main() {
    let e = std::fs::read_to_string("example").expect("example");
    let i = std::fs::read_to_string("input").expect("input");

    println!("P1 e: {:?}", solve_p1(&e));
    println!("P1 i: {:?}", solve_p1(&i));

    println!("P2 e: {:?}", solve_p2(&e));
    println!("P2 i: {:?}", solve_p2(&i));
}
