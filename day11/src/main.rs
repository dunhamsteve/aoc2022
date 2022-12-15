use std::{fs, str::FromStr, num::ParseIntError};

#[derive(Copy,Clone,Debug)]
enum Val { Old, Num(i64) }
impl FromStr for Val {
    type Err = ParseIntError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if s == "old" { 
            Ok(Val::Old)
        } else {
            Ok(Val::Num(s.parse()?))
        }
    }
}

impl Val {
    fn get(&self, old : i64) -> i64 {
        match self {
            Val::Old => old,
            Val::Num(v) => *v,
        }
    }
}
#[derive(Debug)]
struct Monkey {
    items: Vec<i64>,
    op: (bool, Val, Val),
    test: i64,
    dest_t: usize,
    dest_f: usize,
    count: usize,
}

impl FromStr for Monkey {
    type Err = ParseIntError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut test = 0;
        let mut dest_t = 0;
        let mut dest_f = 0;
        let mut op = (false, Val::Old, Val::Old);
        let mut items : Vec<i64> = Vec::new();
        for line in s.split("\n") {
            let parts: Vec<&str> = line.trim().split(' ').collect();
            match parts[..] {
                ["Operation:","new","=",a,b,c] => {
                    op = (b == "*", a.parse().unwrap(), c.parse().unwrap());
                },
                ["Starting", "items:", .. ] => {
                    items = parts[2..].iter().map(|s| s.replace(',', "").parse().unwrap()).collect();
                },
                ["Test:", "divisible", "by", n] => { test = n.parse().unwrap() },
                ["If","true:", "throw","to", "monkey", n] => {
                    dest_t = n.parse().unwrap()
                },
                ["If","false:", "throw","to", "monkey", n] => {
                    dest_f = n.parse().unwrap()
                },
                [""] => {},
                ["Monkey",_] => {},
                _ => { println!("Parse error for: {line}") }
            }
        }
        Ok(Monkey {items, op, test, dest_t, dest_f, count: 0})
    }
}

fn step(monkeys: &mut Vec<Monkey>, adjust: i64) {
    for i in 0..monkeys.len() {
        let items: Vec<i64> = monkeys[i].items.clone();
        let (op,a,b) = monkeys[i].op;
        let test = monkeys[i].test;
        let dest_t = monkeys[i].dest_t;
        let dest_f = monkeys[i].dest_f;
        monkeys[i].items.clear();
        monkeys[i].count += items.len();
        for item in items {
            let a = a.get(item);
            let b = b.get(item);
            let tmp = if op { a * b } else { a + b };
            let new_val = if adjust > 0 { tmp % adjust } else { tmp / 3 };
            if new_val % test == 0 {
                monkeys[dest_t].items.push(new_val)
            } else {
                monkeys[dest_f].items.push(new_val)
            }
        }
    }
}

fn gcd(a: i64, b: i64) -> i64 {
    let mut a = a;
    let mut b = b;
    loop {
        let r = a % b;
        if r == 0 { return b; }
        a = b;
        b = r;
    }
}

fn lcm(a: i64, b: i64) -> i64 {
    a  / gcd(a,b) * b
}

fn doit(fname: &str) {
    let content = fs::read_to_string(fname).expect("read file");
    let mut monkeys: Vec<Monkey> = content.split("\n\n").map(|s| s.parse().unwrap()).collect();
    
    for _ in 0..20 {
        step(&mut monkeys, 0);
    }
    let mut tmp : Vec<usize> = monkeys.iter().map(|m| m.count).collect();
    tmp.sort();
    let part1 = tmp.pop().unwrap() * tmp.pop().unwrap();

    let mut monkeys: Vec<Monkey> = content.split("\n\n").map(|s| s.parse().unwrap()).collect();
    let foo = monkeys.iter().map(|m| m.test).reduce(lcm).unwrap();
    println! ("mod {foo}");
    for _ in 0..10000 {
        step(&mut monkeys, foo );
    }
    tmp = monkeys.iter().map(|m| m.count).collect();
    tmp.sort();
    let part2 = tmp.pop().unwrap() * tmp.pop().unwrap();
    
    println!("{fname} part1 {part1} part2 {part2}");
}

fn main() {
    doit("eg.txt");
    doit("input.txt");
}



