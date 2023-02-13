struct I<'a> {
    key: &'a str,
    nzeros: usize,
    c: usize,
}

impl<'a> Iterator for I<'a> {
    type Item = String;
    fn next(&mut self) -> Option<String> {
        loop {
            let c = self.c;
            // always point to the next element to check
            self.c += 1;

            let s = format!("{}{}", self.key, c);
            let v = md5::compute(s.as_bytes());

            let h = format!("{v:x}");

            if h.chars().take(self.nzeros).all(|c| c == '0') {
                return Some(h)
            }
        }
    }
}

impl<'a> I<'a> {
    fn new(key: &'a str, nzeros: usize) -> Self {
        Self {
            key,
            nzeros,
            c: 0,
        }
    }
}

fn solve_p1(i: &str) -> String {
    let mut r = String::new();

    for (ix, v) in I::new(i, 5).take(8).enumerate() {
        println!("{} {}", ix, v);
        // pick 5th hash hexdigit
        r.push_str(&v[5..6]);
    }

    r
}

fn solve_p2(i: &str) -> String {
    let mut r: [char; 8] = ['_'; 8];
    let mut found = 0;

    for v in I::new(i, 5) {
        let dix = usize::from_str_radix(&v[5..6], 16).expect("a hexdigit");
        if dix < r.len() && r[dix] == '_' {
            let c = v.chars().nth(6).expect("a digit");
            r[dix] = c;
            found += 1;

            println!("{}", String::from_iter(&r));

            if found == r.len() { break }
        }
    }

    String::from_iter(&r)
}

fn main() {
    println!("P1 example: {}", solve_p1("abc"));
    println!("P1 ans: {}", solve_p1("reyedfim"));
    println!("P2 example: {}", solve_p2("abc"));
    println!("P2 ans: {}", solve_p2("reyedfim"));
}
