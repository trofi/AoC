use std::collections::BTreeSet;
use std::collections::BinaryHeap;

type Map = BTreeSet<(isize, &'static str)>;

fn prio(m: &Map, steps: isize) -> isize {
    - (2 * steps + m.iter().map(|v| 4 - v.0).sum::<isize>())
}

fn satisfies_constraint(m: &Map) -> bool {
    // Each microchip must be either without generators
    // on the floor or with powering matching generator:
    let mut is_satisfied = true;

    for f in 1..=4 {
        let mut gs: BTreeSet<char> = BTreeSet::new();
        let mut ms: BTreeSet<char> = BTreeSet::new();
        for (fl, e) in m {
            if f != *fl { continue }
            let vs: Vec<char> = e.chars().collect();
            match vs.as_slice() {
                [t, 'G'] => { gs.insert(*t); },
                [t, 'M'] => { ms.insert(*t); },
                _        => panic!("Unexpected {:?}", vs),
            }
        }
        for m in &ms {
            if gs.contains(&m) { continue }
            if gs.is_empty() { continue }
            is_satisfied = false;
        }
    }

    is_satisfied
}

fn solve(i: &Map) -> isize {

    #[derive(Debug, Eq, Ord, PartialEq, PartialOrd)]
    struct Key {
        prio: isize,
        steps: isize,
        elev: isize,
        map: Map,
    }

    let mut q: BinaryHeap<Key> = BinaryHeap::new();
    q.push(Key{
        prio: prio(i, 0),
        steps: 0,
        elev: 1,
        map: i.clone(),
    });

    let mut v: BTreeSet<(isize, Map)> = BTreeSet::new();

    while let Some(k) = q.pop() {
        // already seen this configuration
        if v.contains(&(k.elev, k.map.clone())) { continue }
        v.insert((k.elev, k.map.clone()));

        let steps = k.steps + 1;

        // solved!
        if k.map.iter().all(|(f, _)| *f == 4) {
            return k.steps
        }

        // move the elevator up or down one step
        for elev in [k.elev - 1, k.elev + 1] {
            if !(1..=4).contains(&elev) { continue }

            let on_floor1: Vec<&'static str> =
                k.map.iter()
                     .filter(|(f, _)| *f == k.elev)
                     .map(|(_, v)| *v)
                     .collect();
            // pick 1 element
            for e1 in &on_floor1 {
                let mut map1 = k.map.clone();
                map1.remove(&(k.elev, e1));
                map1.insert((elev, e1));
                if satisfies_constraint(&map1) {
                    q.push(Key{
                        prio: prio(&map1, steps),
                        steps,
                        elev,
                        map: map1.clone(),
                    });
                }

                // pick 2 element
                let on_floor2: Vec<&'static str> =
                    map1.iter()
                        .filter(|(f, _)| *f == k.elev)
                        .map(|(_, v)| *v)
                        .collect();

                for e2 in &on_floor2 {
                    let mut map2 = map1.clone();
                    map2.remove(&(k.elev, e2));
                    map2.insert((elev, e2));
                    if satisfies_constraint(&map2) {
                        q.push(Key{
                            prio: prio(&map2, steps),
                            steps,
                            elev,
                            map: map2,
                        });
                    }
                }
            }
        }
    }

    panic!("No solutions!");
}

fn main() {
    let example: Map = Map::from([
        (1, "HM"), (1, "LM"),
        (2, "HG"),
        (3, "LG"),
    ]);
    let input1: Map = Map::from([
        (1, "SG"), (1, "SM"), (1, "PG"), (1, "PM"),
        (2, "TG"), (2, "RG"), (2, "RM"), (2, "CG"), (2, "CM"),
        (3, "TM"),
    ]);
    let input2: Map = Map::from([
        (1, "SG"), (1, "SM"), (1, "PG"), (1, "PM"),
        /* extra */ (1, "EG"), (1, "EM"), (1, "DG"), (1, "DM"),
        (2, "TG"), (2, "RG"), (2, "RM"), (2, "CG"), (2, "CM"),
        (3, "TM"),
    ]);
    println!("P1 example: {:?}", solve(&example));
    println!("P1 ans: {:?}", solve(&input1));
    println!("P2 ans: {:?}", solve(&input2));
}
