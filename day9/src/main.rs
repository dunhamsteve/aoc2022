use std::{fs, str::FromStr, num::ParseIntError, collections::HashSet};

enum Dir { Up, Down, Left, Right }

fn parse_line(line: &str) -> (Dir, i32) {
    let (a,b) = line.split_once(' ').unwrap();
        let dir = match a {
            "U" => Dir::Up, "D" => Dir::Down, "L" => Dir::Left, _ => Dir::Right,
        };
    (dir, b.parse().unwrap())
}

fn move_point(pt : &mut (i32,i32), dir: &Dir) {
    match dir {
        Dir::Up => pt.1 -= 1,
        Dir::Down => pt.1 += 1,
        Dir::Left => pt.0 -= 1,
        Dir::Right => pt.0 += 1,
    }
}

fn run(instrs: &Vec<(Dir,i32)>, n: usize) -> usize {
    let mut visited: HashSet<(i32,i32)> = HashSet::new();
    let mut knots: Vec<(i32,i32)> = Vec::new();
    knots.resize(n, (0,0));

    for (d,v) in instrs {
        for _ in 0..*v {
            move_point(&mut knots[0], &d);
            for i in 1..knots.len() {
                let (a,b) = knots[i-1];
                let (mut x, mut y) = knots[i];

                if a < x { x -= 1 }
                if a > x { x += 1 }
                if b < y { y -= 1 }
                if b > y { y += 1}

                if (a,b) != (x,y) {  knots[i] = (x,y) }
            }
            visited.insert(*knots.last().unwrap());
        }
    }
    visited.len()
}

fn doit(fname: &str) {
    let content = fs::read_to_string(fname).expect("read file");
    let instrs: Vec<(Dir,i32)> = content.lines().map(parse_line).collect();
    let p1 = run(&instrs, 2);
    let p2 = run(&instrs, 10);
    println!("{fname} part1 {p1} part2 {p2}");
}

fn main() {
    doit("eg.txt");
    doit("input.txt");
}
