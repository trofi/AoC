use std::{*, collections::*, fs::*, io::*, iter::*};

type P = (isize, isize, isize);
type D = (usize, usize, usize);

#[derive(Debug)]
struct Scanner {
    ix: usize,
    points: BTreeSet<P>,
}

fn get_input(input_file: &str) -> Vec<Scanner> {
    let mut reader = BufReader::new(File::open(input_file).unwrap());

    let mut ns = String::new();
    println!("read: {}", reader.read_to_string(&mut ns).expect("IO error?"));

    let mut res = Vec::new();

    for l in ns.split("\n\n") {
        // --- scanner <N> ----
        // d,d,d
        let mut li = l.lines();
        let v = Vec::from_iter(li.next().expect("at least one").split(' '));
        let ix = v[2].parse::<usize>().expect("number");

        let mut ps = BTreeSet::new();
        for xyz in li {
            let v = Vec::from_iter(xyz.split(',').map(|e| e.parse::<isize>().expect("number")));
            ps.insert((v[0], v[1], v[2]));
        }

        res.push(Scanner{ix: ix, points: ps});
    }

    return res;
}

fn rx(&(x,y,z): &P) -> P { ( x,-z, y) }
fn ry(&(x,y,z): &P) -> P { (-z, y, x) }
fn rz(&(x,y,z): &P) -> P { (-y, x, z) }

fn rxyz(p: &P, &(nx, ny, nz): &D) -> P {
    let mut pp = p.clone();

    for _ in 0..nx {
        pp = rx(&pp);
        for _ in 0..ny {
            pp = ry(&pp);
            for _ in 0..nz {
                pp = rz(&pp);
            }
        }
    }

    return pp;
}

fn main() {
    for input_file in ["example", "input"] {
        let input = get_input(input_file);

        let mut disposition = BTreeSet::<((usize, usize), P, D)>::new();

        for ls in input.iter() {
            for rs in input.iter() {
                if ls.ix == rs.ix { continue; }

                'rotas:
                for nx in 0..4 {
                    for ny in 0..4 {
                        for nz in 0..4 {
                            let d = (nx, ny, nz);
                            let mrps = BTreeSet::<P>::from_iter(
                                rs.points.iter().map(|p| rxyz(p, &d))
                            );
                            for lp in ls.points.iter() {
                                for mrp in mrps.iter() {
                                    let mut matches = 0;
                                    let mut mismatches = 0;
                                    let ds = (lp.0 - mrp.0,
                                              lp.1 - mrp.1,
                                              lp.2 - mrp.2);

                                    // skip early, already present
                                    if disposition.contains(&((ls.ix, rs.ix), ds, d)) { continue; }

                                    for mrpc in mrps.iter() {
                                        let lppc = (mrpc.0 + ds.0,
                                                    mrpc.1 + ds.1,
                                                    mrpc.2 + ds.2);
                                        if ls.points.contains(&lppc) {
                                            matches += 1;
                                        } else if lppc.0.abs() < 1000 && lppc.1.abs() < 1000 && lppc.2.abs() < 1000 {
                                            mismatches += 1;
                                            break;
                                        }
                                    }
                                    if matches >= 6 && mismatches == 0 {
                                        disposition.insert(((ls.ix, rs.ix), ds, d));
                                        println!("found: {:>3} -- {:<3} ({:?}) ms: {} mm: {}", ls.ix, rs.ix, d, matches, mismatches);
                                        break 'rotas;
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }

        let mut resolvers = BTreeMap::<usize,Option<(usize, P, (usize, usize, usize))>>::new();
        // Base point.
        resolvers.insert(0, None);

        while resolvers.len() != input.len() {
            let mut found = false;
            for ((li, ri), ds, (nx, ny, nz)) in disposition.iter() {
                if resolvers.contains_key(ri) { continue; }

                if resolvers.contains_key(li) {
                    println!("{:>3} -- {:<3}", li, ri);
                    resolvers.insert(*ri, Some((*li, *ds, (*nx, *ny, *nz))));
                    found = true;
                    break;
                }
            }

            if !found {
                println!("Missing: {:?}", Vec::from_iter(input.iter().map(|e| e.ix).filter(|e| !resolvers.contains_key(e))));
                unreachable!("Not a single-conected component?");
            }
        }

        let mut field = BTreeSet::<P>::new();

        for i in input.iter() {
            //if !resolvers.contains_key(&i.ix) { continue; }
            for p in i.points.iter() {
                let mut mp = p.clone();
                let mut mix = i.ix;
                while let Some((li, ds, d)) = resolvers.get(&mix).expect("resolves") {
                    mp = rxyz(&mp, d);
                    mp.0 += ds.0;
                    mp.1 += ds.1;
                    mp.2 += ds.2;
                    mix = *li;
                }
                field.insert(mp);
            }
        }

        // 323
        println!("{}: {}", input_file, field.len());
    }
}
