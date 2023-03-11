use std::collections::HashMap;

type HashCache = HashMap<usize, md5::Digest>;

fn hash(salt: &str, n: usize, stretch: bool, hc: &mut HashCache) -> md5::Digest {
    if let Some(v) = hc.get(&n) {
        return *v;
    }

    let mut h = md5::compute(format!("{salt}{n}"));
    if stretch {
        for _ in 0..2016 {
            h = md5::compute(format!("{h:x}"));
        }
    }

    hc.insert(n, h);

    h
}

struct MD5Iter<'a> {
    salt: &'a str,
    n: usize,
    stretch: bool,
    hc: &'a mut HashCache,
}

impl<'a> MD5Iter<'a> {
    fn new(salt: &'a str, n: usize, stretch: bool, hc: &'a mut HashCache) -> Self {
        MD5Iter{
            salt,
            n,
            stretch,
            hc,
        }
    }
}

impl<'a> Iterator for MD5Iter<'a> {
    type Item = (String, usize);

    fn next(self: &mut Self) -> Option<Self::Item> {
        let h = hash(self.salt, self.n, self.stretch, &mut self.hc);
        let r = Some((format!("{h:x}"), self.n));
        self.n += 1;
        r
    }
}

struct KeyIter<'a> {
    md5i: MD5Iter<'a>,
}

impl<'a> KeyIter<'a> {
    fn new(salt: &'a str, stretch: bool, hc: &'a mut HashCache) -> Self {
        KeyIter{
            md5i: MD5Iter::new(salt, 0, stretch, hc),
        }
    }
}

impl<'a> Iterator for KeyIter<'a> {
    type Item = usize;

    fn next(self: &mut Self) -> Option<Self::Item> {
        //for (s, n) in &mut self.md5i {
        while let Some((s, n)) = self.md5i.next() {
            let cv: Vec<char> = s.chars().collect();
            let di = cv.windows(3)
                       .filter(|v| v[0] == v[1] && v[1] == v[2]).next();
            if let Some(t) = di {
                let c = t[0];
                let ic = MD5Iter::new(self.md5i.salt, n + 1, self.md5i.stretch, self.md5i.hc);

                let b = ic.take(1000)
                          .any(|(s, _)| {
                              let cv: Vec<char> = s.chars().collect();
                              let di = cv.windows(5)
                                         .filter(|v|
                                                    v[0] == c
                                                 && v[1] == c
                                                 && v[2] == c
                                                 && v[3] == c
                                                 && v[4] == c
                                         ).next();
                              di.is_some()
                          });
                if b {
                    return Some(n)
                }
            }
        }
        return None
    }
}

fn solve(salt: &str, stretch: bool) -> usize {
    let mut hc: HashCache = HashCache::new();
    KeyIter::new(salt, stretch, &mut hc).take(64).last().expect("value")
}

fn main() {
    println!("P1 example: {:?}", solve("abc", false));
    println!("P1 ans: {:?}", solve("zpqevtbw", false));
    println!("P2 ans: {:?}", solve("zpqevtbw", true));
}
