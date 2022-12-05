use std::fs;
use std::num::ParseIntError;
use std::str::FromStr;
use std::cmp::{max, min};


struct Range {
    start: i32,
    end: i32
}

impl Range {
    fn includes(&self, other: &Range) -> bool {
        self.start <=other.start && self.end >= other.end
    }
    fn overlaps(&self, other: &Range) -> bool {
        max(self.start, other.start) <= min(self.end, other.end)
    }
}

impl FromStr for Range {
    type Err = ParseIntError;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let (x,y) = s
            .split_once('-')
            .unwrap();
        let start = x.parse()?;
        let end = y.parse()?;
        Ok(Range { start: start, end: end })
    }
}


fn doit(fname: &str) {
    let content = fs::read_to_string(fname)
        .expect("can't read file");
    let mut count = 0;
    let mut count2 = 0;
    for line in content.lines() {
        let (a,b) = line.split_once(',').unwrap();
        let aa: Range = a.parse().unwrap();
        let bb: Range = b.parse().unwrap();
        if aa.includes(&bb) || bb.includes(&aa) { count += 1 }
        if aa.overlaps(&bb) { count2 += 1 }
    }
    println!("{fname} {count} {count2}")
}

fn main() {
    doit("eg.txt");
    doit("input.txt");
}
