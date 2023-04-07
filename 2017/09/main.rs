enum Node {
    Text(usize),
    Group(Vec<Node>),
}

fn parse_group<'a>(i: &'a str) -> (Node, &'a str) {
    assert!(!i.is_empty());
    let mut v = vec![];
    let mut p = 0;

    loop {
        let prefix = &i[p..p+1];
        match prefix {
            "}" => break,
            "," => { p += 1 },
            _ => {
                let (n, rest) = parse_node(&i[p..]);
                p = i.len() - rest.len();
                v.push(n);
            },
        }
    }
    assert!(&i[p..p+1] == "}");
    p += 1;
    (Node::Group(v), &i[p..])
}

fn parse_garbage<'a>(i: &'a str) -> (Node, &'a str) {
    assert!(!i.is_empty());
    let mut p = 0;
    let mut chars = 0;
    loop {
        let prefix = &i[p..p+1];
        match prefix {
            ">" => break,
            "!" => p += 2,
            _   => { p += 1; chars += 1; },
        }
    }
    assert!(&i[p..p+1] == ">");
    p += 1;
    (Node::Text(chars), &i[p..])
}

fn parse_node<'a>(i: &'a str) -> (Node, &'a str) {
    assert!(!i.is_empty());
    match &i[0..1] {
        "{" => parse_group(&i[1..]),
        "<" => parse_garbage(&i[1..]),
        _ => panic!("Unexpected node start: '{}'", &i[0..std::cmp::min(10, i.len())]),
    }
}

fn parse<'a>(i: &'a str) -> Node {
    let (n, rest) = parse_node(i);
    assert!(rest.is_empty() || rest == "\n");
    n
}

fn depth(n: &Node, d: usize) -> usize {
    match n {
        Node::Text(_) => 0,
        Node::Group(v) => d + v.iter().map(|e| depth(e, d + 1)).sum::<usize>(),
    }
}

fn solve_p1(i: &str) -> usize {
    let t = parse(i);

    depth(&t, 1)
}

fn cnt(n: &Node) -> usize {
    match n {
        Node::Text(c) => *c,
        Node::Group(v) => v.iter().map(|e| cnt(e)).sum::<usize>(),
    }
}

fn solve_p2(i: &str) -> usize {
    let t = parse(i);

    cnt(&t)
}

fn main() {
    let e = std::fs::read_to_string("input").expect("input");

    println!("P1 ans: {}", solve_p1(&e));
    println!("P2 ans: {}", solve_p2(&e));
}
