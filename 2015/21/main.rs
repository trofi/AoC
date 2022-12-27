struct Item {
  _name: &'static str,
  cost: isize,
  damage: isize,
  armor: isize,
}

struct Character {
  hp: isize,
  damage: isize,
  armor: isize,
}

fn can_win(hero: &Character, boss: &Character) -> bool {
  let hero_damage = std::cmp::max(1, hero.damage - boss.armor);
  let boss_damage = std::cmp::max(1, boss.damage - hero.armor);

  let hero_rounds = (boss.hp + hero_damage - 1) / hero_damage;
  let boss_rounds = hero_rounds - 1;

  hero.hp > boss_rounds * boss_damage
}

fn solve(win: bool) -> isize {
  let weapons = [
    Item { _name: "Dagger",     cost:   8, damage: 4, armor: 0, },
    Item { _name: "Shortsword", cost:  10, damage: 5, armor: 0, },
    Item { _name: "Warhammer",  cost:  25, damage: 6, armor: 0, },
    Item { _name: "Longsword",  cost:  40, damage: 7, armor: 0, },
    Item { _name: "Greataxe",   cost:  74, damage: 8, armor: 0, },
  ];

  let armor = [
    Item { _name: "None",       cost:   0, damage: 0, armor: 0, },

    Item { _name: "Leather",    cost:  13, damage: 0, armor: 1, },
    Item { _name: "Chainmail",  cost:  31, damage: 0, armor: 2, },
    Item { _name: "Splintmail", cost:  53, damage: 0, armor: 3, },
    Item { _name: "Bandedmail", cost:  75, damage: 0, armor: 4, },
    Item { _name: "Platemail",  cost: 102, damage: 0, armor: 5, },
  ];

  let rings = [
    Item { _name: "None 1",     cost:   0, damage: 0, armor: 0, },
    Item { _name: "None 2",     cost:   0, damage: 0, armor: 0, },

    Item { _name: "Damage +1",  cost:  25, damage: 1, armor: 0, },
    Item { _name: "Damage +2",  cost:  50, damage: 2, armor: 0, },
    Item { _name: "Damage +3",  cost: 100, damage: 3, armor: 0, },
    Item { _name: "Defense +1", cost:  20, damage: 0, armor: 1, },
    Item { _name: "Defense +2", cost:  40, damage: 0, armor: 2, },
    Item { _name: "Defense +3", cost:  80, damage: 0, armor: 3, },
  ];

  let boss = Character {
    hp: 100,
    damage: 8,
    armor: 2,
  };

  let mut r = if win { isize::MAX } else { isize::MIN } ;

  for w in &weapons {
    for a in &armor {
      for r1 in &rings {
        for r2 in &rings {
          let equip = [w, a, r1, r2];

          let hero = Character {
             hp:     100,
             damage: equip.iter().map(|e| e.damage).sum(),
             armor:  equip.iter().map(|e| e.armor).sum(),
          };
          if win {
            if can_win(&hero, &boss) {
              let cost = equip.iter().map(|e| e.cost).sum();
              r = std::cmp::min(r, cost);
            }
          } else {
            if !can_win(&hero, &boss) {
              let cost = equip.iter().map(|e| e.cost).sum();
              r = std::cmp::max(r, cost);
            }
          }
        }
      }
    }
  }

  r

}

fn main() {
  println!("P1 ans: {:?}", solve(true));
  println!("P2 ans: {:?}", solve(false));
}
