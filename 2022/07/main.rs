use std::fs::read_to_string;
use std::error::Error;
use std::collections::HashMap;
use std::collections::HashSet;

type E = Box<dyn Error>;

#[derive(Debug)]
enum Entry {
  File{
    size: usize,
  },
  Dir{
    entries: HashSet<String>,
  },
}

#[derive(Debug)]
struct FS {
  cwd: String,
  table: HashMap<String, Entry>,
}

impl FS {
  fn new() -> Self {
    FS{
      cwd: "".to_string(),
      table: HashMap::new(),
    }
  }

  fn to_abs(self: &Self, p: &str) -> String {
    let mut d: Vec<_> = self.cwd.split('/').collect();

    for c in p.split('/') {
      match &c {
        &".." => { d.pop(); },
        v     => { d.push(v); },
      }
    }

    d.into_iter().filter(|e| !e.is_empty()).collect::<Vec<_>>().join("/")
  }

  fn cd(self: &mut Self, d: &str) {
    let fd = self.to_abs(d);

    if self.table.get(&fd).is_none() {
      self.table.insert(fd.clone(), Entry::Dir{entries: HashSet::new()});
    }

    self.cwd = fd;
  }

  fn mkdir(self: &mut Self, d: &str) {
    match self.table.get_mut(&self.cwd).expect("expect dir to exist") {
      Entry::File{..} => panic!("Can't create {} directory in {} file", d, self.cwd),
      Entry::Dir{ref mut entries} => {
        entries.insert(d.to_string());
        let fd = self.to_abs(d);
        let prev = self.table.insert(fd.clone(), Entry::Dir{entries: HashSet::new()});
        if prev.is_some() {
          panic!("Can't create {} directory, {:?} already exists", fd, prev);
        }
      },
    }
  }

  fn mkfile(self: &mut Self, f: &str, size: usize) {
    match self.table.get_mut(&self.cwd).expect("dir") {
      Entry::File{..} => panic!("TODO: allow idempotent insertion of {} to {}", f, self.cwd),
      Entry::Dir{ref mut entries} => {
        entries.insert(f.to_string());
        let ff = self.to_abs(f);
        let prev = self.table.insert(ff.clone(), Entry::File{size: size});
        if prev.is_some() {
          panic!("Can't create {} file, {:?} already exists", ff, prev);
        }
      },
    }
  }

  fn size(self: &Self, e: &str) -> usize {
    match self.table.get(e).expect("size: entry") {
      Entry::File{size} => *size,
      Entry::Dir{ref entries} => {
        entries.iter().map(|f| {
          let ff = if e.is_empty() { f.to_string() } else { format!("{}/{}", e, f) };
          self.size(&ff)
        }).sum()
      },
    }
  }
}

fn parse(i: &Vec<&str>) -> FS {
  let mut fs = FS::new();

  for l in i.iter() {
    let cmd = l.split(' ').collect::<Vec<_>>();
    match cmd.as_slice() {
      /* commands */
      &["$", "cd", d] => fs.cd(d),
      &["$", "ls"] => (),

      /* outputs */
      &["dir", d] => fs.mkdir(d),
      &[ss, f] if ss.parse::<usize>().is_ok() => fs.mkfile(f, ss.parse().expect("an int")),
      _ => panic!("Unknown input: '{}'", l)
    }
  }

  fs
}

fn solve_p1(fs: &FS) -> usize {
  let mut dsizes: Vec<_> = fs.table.iter().map(|(k,v)|
    match v {
      Entry::File{..} => 0,
      Entry::Dir{..} => fs.size(k),
    }
  ).filter(|e| *e <= 100_000).collect();

  dsizes.sort();
  dsizes.iter().sum()
}

fn solve_p2(fs: &FS) -> usize {
  let mut dsizes: Vec<_> = fs.table.iter().map(|(k,v)|
    match v {
      Entry::File{..} => 0,
      Entry::Dir{..} => fs.size(k),
    }
  ).collect();
  let free_space = 70_000_000 - fs.size("");
  let need_extra = 30_000_000 - free_space;

  dsizes.sort();
  *dsizes.iter().filter(|e| **e >= need_extra).next().expect("at least one")
}

fn main() -> Result<(), E> {
  for ifile in ["example" , "input"] {
    let input_string = read_to_string(ifile)?;
    let input = input_string.lines().collect();

    let fs = parse(&input);

    // 198712 is wrong
    let ans_p1 = solve_p1(&fs);
    println!("{}: P1 ans: {:?}", ifile, ans_p1);
    let ans_p2 = solve_p2(&fs);
    println!("{}: P2 ans: {:?}", ifile, ans_p2);
  }
  Ok(())
}
