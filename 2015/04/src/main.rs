fn brute5(i: &str) -> usize {
  for c in 0.. {
    let s = format!("{}{}", i, c);
    let v = md5::compute(s.as_bytes());
    // could be implemented better by comparing to a number
    let sv = format!("{:x}", v);
    if sv.starts_with("00000") {
      return c;
    }
  }

  0
}

fn brute6(i: &str) -> usize {
  for c in 0.. {
    let s = format!("{}{}", i, c);
    let v = md5::compute(s.as_bytes());
    // could be implemented better by comparing to a number
    let sv = format!("{:x}", v);
    if sv.starts_with("000000") {
      return c;
    }
  }

  0
}

fn main() {
  for i in ["abcdef", "pqrstuv", "yzbqklnj"] {
    println!("{}: 5={}, 6={}", i, brute5(i), brute6(i));
  }
}
