fn solve_p1(n: u32) -> u32 {
    let mut v: Vec<u32> = Vec::new();
    v.resize(n as usize, 0);

    for i in 0..n {
        v[i as usize] = (i + 1) % n;
    }
    let mut p = 0;

    while p != v[p as usize] {
        let np = v[v[p as usize] as usize];
        v[p as usize] = np;
        p = np;
    }

    p + 1
}

fn solve_p2(n: u32) -> u32 {
    let mut v: Vec<u32> = Vec::new();
    v.resize(n as usize, 0);

    for i in 0..n {
        v[i as usize] = (i + 1) % n;
    }
    let mut l = n;
    let mut p = 0;

    while p != v[p as usize] {
        let mut victim = p;
        for _ in 0..(l/2 - 1) {
            victim = v[victim as usize];
        }
        v[victim as usize] = v[v[victim as usize] as usize];

        p = v[p as usize];
        l -= 1;
        if l % 10000 == 0 {
            println!("{} iters left", l);
        }
    }

    p + 1
}


fn main() {
    println!("P1 example: {}", solve_p1(5));
    println!("P1 ans: {}", solve_p1(3001330));
    println!("P2 example: {}", solve_p2(5));
    println!("WARNING: this is a naive solution, that takes an hour to complete!");
    println!("P2 ans: {}", solve_p2(3001330));
}
