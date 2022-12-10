use std::fs;

enum Dir { Up, Down, Left, Right }

struct Forest {
    heights: Vec<Vec<u8>>,
    height: usize,
    width: usize,
}

struct ForestIter<'a> {
    forest: &'a Forest,
    row: usize,
    col: usize,
    dir: Dir,
}

impl Forest {
    fn iter(&self, row: usize, col: usize, dir: Dir) -> ForestIter<'_> {
        ForestIter { forest: self, row, col, dir}
    }
    fn visible(&self, row: usize, col: usize, dir: Dir) -> bool {
        let height = self.heights[row][col];
        self.iter(row,col,dir).all(| h |  h < height)
    }
    fn count(&self, row: usize, col: usize, dir: Dir) -> i32 {
        let height = self.heights[row][col];
        let mut count = 0;
        for h in self.iter(row,col,dir) {
            if h <= height { count += 1 }
            if h >= height { break }
        }
        return count
    }
}

// Maybe overkill? but pedogical purposes...
impl<'a> Iterator for ForestIter<'a> {
    // row, col, height - actually only need height...
    type Item = u8; 

    fn next(&mut self) -> Option<Self::Item> {
        let row = &mut self.row;
        let col = &mut self.col;
        match &self.dir {
            Dir::Up => if *row > 0 { *row -= 1 } else { return None },
            Dir::Down => if *row + 1 < self.forest.height { *row += 1 } else { return None},
            Dir::Left => if *col > 0 { *col -= 1 } else { return None },
            Dir::Right => if *col +1 < self.forest.width { *col += 1 } else {return None},
        }
        Some (self.forest.heights[*row][*col])
    }
}

fn parse_forest(content: String) -> Forest {
    let mut heights: Vec<Vec<u8>> = Vec::new();
    for line in content.lines() {
        let row: Vec<u8> = line.as_bytes().iter().map(|c| c - 0x30).collect();
        heights.push(row);
    }
    let width = heights[0].len();
    let height = heights.len();
    Forest { heights, height, width }
}

fn doit(fname: &str) {
    let content = fs::read_to_string(fname).expect("read file");
    let f = parse_forest(content);

    // part1: visible from outside
    // rust version, I'll just walk the whole thing.

    let mut visible = 0;
    let mut max = 0;
    for r in 0..f.height {
        for c in 0..f.width {
            if f.visible(r,c,Dir::Up)
                || f.visible(r, c, Dir::Down)
                || f.visible(r, c, Dir::Left)
                || f.visible(r, c, Dir::Right) {
                    visible += 1
                }
            
            let score =
                  f.count(r,c,Dir::Up)
                * f.count(r,c,Dir::Down)
                * f.count(r,c,Dir::Left)
                * f.count(r,c,Dir::Right);
            if score > max { max = score }
        }
    }
    println!("{fname} part1 {visible} part2 {max}")
}

fn main() {
    doit("eg.txt");
    doit("input.txt");
}
