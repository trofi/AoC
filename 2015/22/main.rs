use std::collections::BinaryHeap;
use std::collections::BTreeSet;

#[derive(Eq, Ord, PartialEq, PartialOrd, Clone)]
struct Character {
    hp: isize,
    mana: isize,
    damage: isize,
    armor: isize,
}

#[derive(Eq, Ord, PartialEq, PartialOrd)]
struct Spell {
    name: &'static str,
    cost: isize,
    length: isize,
    damage: isize,
    buff_damage: isize,
    buff_armor: isize,
    heal: isize,
    buff_heal: isize,
    buff_charge: isize,
}

const SPELLS: &'static [Spell] = &[
    Spell {
        name: "Magic Missile",
        cost:        53,
        length:      0,
        damage:      4,
        buff_damage: 0,
        buff_armor:  0,
        heal:        0,
        buff_heal:   0,
        buff_charge: 0,
    },
    Spell {
        name: "Drain",
        cost:        73,
        length:      0,
        damage:      2,
        buff_damage: 0,
        buff_armor:  0,
        heal:        2,
        buff_heal:   0,
        buff_charge: 0,
    },
    Spell {
        name: "Shield",
        cost:        113,
        length:      6,
        damage:      0,
        buff_damage: 0,
        buff_armor:  7,
        heal:        0,
        buff_heal:   0,
        buff_charge: 0,
    },
    Spell {
        name: "Poison",
        cost:        173,
        length:      6,
        damage:      0,
        buff_damage: 3,
        buff_armor:  0,
        heal:        0,
        buff_heal:   0,
        buff_charge: 0,
    },
    Spell {
        name: "Recharge",
        cost:        229,
        length:      5,
        damage:      0,
        buff_damage: 0,
        buff_armor:  0,
        heal:        0,
        buff_heal:   0,
        buff_charge: 101,
    },
];

fn solve(d: isize) -> isize {
    let hero = Character {
        hp: 50,
        mana: 500,
        damage: 0,
        armor: 0,
    };

    let boss = Character {
        hp: 58,
        mana: 0,
        damage: 9,
        armor: 0,
    };

    let mut r = isize::MAX;

    #[derive(Eq, Ord, PartialEq, PartialOrd, Clone)]
    struct Key {
        prio: isize, // -(mana_spent + bos_health)
        mana_spent: isize,
        hero: Character,
        boss: Character,
        buffs: Vec<isize>,
        hero_moves: bool,
    }
    let mut q: BinaryHeap<Key> = BinaryHeap::new();
    let mut visited: BTreeSet<Key> = BTreeSet::new();
    let mut buffs: Vec<isize> = Vec::new();
    buffs.resize(SPELLS.len(), 0);
    q.push(Key {
        prio: -(0 + boss.hp),
        mana_spent: 0,
        hero,
        boss,
        buffs,
        hero_moves: true,
    });

    while !q.is_empty() {
        let mut k = q.pop().unwrap();

        if visited.contains(&k) { continue }
        visited.insert(k.clone());

        if k.hero.hp <= 0 { continue }

        // apply buffs
        let mut buff_armor = 0;

        for (ix, b) in k.buffs.iter_mut().enumerate() {
            if *b == 0 { continue }
            let s = &SPELLS[ix];
            *b -= 1;
            buff_armor += s.buff_armor;
            k.boss.hp -= s.buff_damage;
            k.hero.hp += s.buff_heal;
            k.hero.mana += s.buff_charge;
        }
        if k.boss.hp <= 0 {
            if r > k.mana_spent {
                println!("Found better: {} -> {}", r, k.mana_spent);
            }
            r = std::cmp::min(r, k.mana_spent);
            continue
        }
        if k.mana_spent > r { continue }

        for (ix, s) in SPELLS.iter().enumerate() {
            let mut k = k.clone();
            k.hero_moves = !k.hero_moves;
            if !k.hero_moves {
                if s.cost > k.hero.mana { continue }
                if k.buffs[ix] > 0 { continue }

                k.hero.hp -= d;

                // instant effect:
                k.hero.hp += s.heal;
                k.boss.hp -= s.damage;
                k.hero.mana -= s.cost;
                k.mana_spent += s.cost;
                // buffs effect:
                k.buffs[ix] += s.length;

                q.push(k);
            } else {
                k.hero.hp -= std::cmp::max(1, k.boss.damage - buff_armor);
                q.push(k);
            }
        }
    }

    r
}

fn main() {
    println!("P1 ans: {:?}", solve(0));
    println!("P2 ans: {:?}", solve(1));
}
