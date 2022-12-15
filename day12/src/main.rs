use std::{fs, str::FromStr, collections::VecDeque};

struct Grid {
    width: usize,
    height: usize,
    data: Vec<Vec<u8>>
}

impl FromStr for Grid {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut data = Vec::new();
        let mut width = 0;
        for line in s.lines() {
            let x: Vec<u8> = line.bytes().collect();
            width = x.len();
            data.push(x);
        }
        let height = data.len();
        Ok(Grid { width, height, data })
    }
}

type Point = (usize,usize);

impl Grid {
    // I was going to do an iterator for this, but it was a lot more code
    fn neighbors(&self, pt: Point) -> Vec<Point> {
        let mut rval = Vec::new();
        if pt.0 > 0 { rval.push((pt.0-1,pt.1)) }
        if pt.1 > 0 { rval.push((pt.0, pt.1-1)) }
        if pt.0+1 < self.height { rval.push((pt.0+1, pt.1)) }
        if pt.1+1 < self.width { rval.push((pt.0, pt.1+1))}
        return rval
    }
    fn find(&self, val: u8) -> Option<Point> {
        for (r,row) in self.data.iter().enumerate() {
            for (c, v) in row.iter().enumerate() {
                if *v == val { return Some((r,c)) }
            }
        }
        None
    }
    fn get(&self, pt: Point) -> u8 {
        self.data[pt.0][pt.1]
    }
    fn put(&mut self, pt: Point, val: u8) {
        self.data[pt.0][pt.1] = val
    }
}

type Item = (usize, Point);

fn elev(ch: u8) -> u8 {
    if ch == 83 { return 97 }
    if ch == 69 { return 122 }
    return ch
}

fn step(grid: &mut Grid, start: Point, up: bool, goal: u8) -> usize {
    let mut queue : VecDeque<Item> = VecDeque::new();
    queue.push_back((0,start));
    while let Some((dist,pt)) = queue.pop_front() {
        let h = grid.get(pt);
        if h == 254 { continue }
        if h == goal { return dist }
        grid.put(pt, 254); // mark as done
        for cand in grid.neighbors(pt) {
            let x = grid.get(cand);
            if x == 254 { continue }
            let h = elev(h);
            let x = elev(x);
            if up && x <= h + 1 || !up && h <= x + 1 {
                queue.push_back((dist+1,cand));
            }
        }
    }
    unreachable!() // failed to find path
}

fn doit(fname: &str) {
    let content = fs::read_to_string(fname).expect("read file");
    let mut grid: Grid = content.parse().expect("file parses");

    let start = grid.find(83).expect("find start");
    let part1 = step(&mut grid, start, true, 69);

    let mut grid: Grid = content.parse().expect("file parses");
    let end = grid.find(69).expect("find end");
    let part2 = step(&mut grid, end, false, 97);

    println!("{fname} part1 {part1} part2 {part2}");
}

fn main() {
    doit("eg.txt");
    doit("input.txt");
}
