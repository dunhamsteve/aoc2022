use std::fs;

// I'm using n array instead of hashmap for the rust version,
// I'm going to assume 0-1000 is wide enough for x.  HashMap has the
// advantage of being open ended
#[derive(Debug)]
struct Grid {
    data: Vec<bool>,
    size: usize,
}

impl Grid {
    fn get(&self, pt: Point) -> bool {
        self.data[pt.0 + pt.1 * 1024]
    }
    fn set(&mut self, pt: Point) {
        self.data[pt.0 + pt.1 * 1024] = true
    }
    fn make(size: usize) -> Grid {
        let mut data: Vec<_> = Vec::new();
        data.resize(size * 1024, false);
        Grid { size, data }
    }
    fn drop(&mut self, pt: Point) -> Point {
        let mut pt = pt;
        loop {
            if pt.1 == self.size - 1 {
                self.set(pt);
                return pt;
            } else if !self.get((pt.0, pt.1 + 1)) {
                pt = (pt.0, pt.1 + 1)
            } else if !self.get((pt.0 - 1, pt.1 + 1)) {
                pt = (pt.0 - 1, pt.1 + 1)
            } else if !self.get((pt.0 + 1, pt.1 + 1)) {
                pt = (pt.0 + 1, pt.1 + 1)
            } else {
                self.set(pt);
                return pt;
            }
        }
    }
}

type Point = (usize, usize);

fn parse_pair(s: &str) -> Point {
    let mut tmp = s.trim().split(",").map(|s| s.parse().unwrap());
    (tmp.next().unwrap(), tmp.next().unwrap())
}

fn parse_line(line: &str) -> Vec<Point> {
    line.split("->").map(parse_pair).collect()
}

fn make_grid(content: &str) -> Grid {
    let rows: Vec<_> = content.lines().map(parse_line).collect();
    let maxy = rows.iter().flatten().map(|s| s.1).max().unwrap();

    // allow maxy coord and one bigger
    let mut grid = Grid::make(maxy + 2);
    for chunk in rows {
        for i in 1..chunk.len() {
            let a = chunk[i - 1];
            let b = chunk[i];
            if a.0 == b.0 {
                let (s, e) = if a.1 < b.1 {
                    (a.1, b.1 + 1)
                } else {
                    (b.1, a.1 + 1)
                };
                for y in s..e {
                    grid.set((a.0, y))
                }
            } else {
                let (s, e) = if a.0 < b.0 {
                    (a.0, b.0 + 1)
                } else {
                    (b.0, a.0 + 1)
                };
                for x in s..e {
                    grid.set((x, a.1));
                }
            }
        }
    }
    grid
}

fn doit(fname: &str) {
    let content = fs::read_to_string(fname).expect("read file");

    // part1
    let mut grid = make_grid(&content);
    let mut count = 0;
    loop {
        let end = grid.drop((500, 0));
        if end.1 == grid.size - 1 {
            break;
        }
        count += 1;
    }
    println!("{fname} part1 {count}");

    // part2 - continuing previous drop
    count += 1; // we didn't count it for part1, but it's there
    loop {
        count += 1;
        let end = grid.drop((500, 0));
        if end == (500, 0) {
            break;
        }
    }
    println!("{fname} part2 {count}")
}

fn main() {
    doit("eg.txt");
       doit("input.txt");
}
