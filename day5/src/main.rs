
use std::cmp::max;
use std::fs;

#[derive(Debug)]
struct Tableau {
    data: Vec<Vec<char>>
}


impl Tableau {
    fn new() -> Tableau { 
        return Tableau { data: Vec::new() }
    }
    fn ensure(&mut self, col: usize) {
        while self.data.len() <= col { self.data.push(Vec::new()) }
    }
    fn push(&mut self, col: usize, ch: char) {
        self.ensure(col);
        self.data[col].push(ch)
    }
    fn reverse(&mut self) {
        for i in 0..self.data.len() {
            self.data[i].reverse()
        }
    }
    // move is a reserved name..
    fn change(&mut self, count: usize, from: usize, to: usize, part2: bool) {
        self.ensure(max(from,to));
        if part2 { 
            let pt = self.data[from].len() - count;
            let tail: Vec<char> = self.data[from][pt..].iter().map(|&x| x).collect();
            
            self.data[to].extend(tail);        
            self.data[from].truncate(pt);
        } else {
            for i in 0..count {
                let c = self.data[from].pop().unwrap();
                self.data[to].push(c)
            }    
        }        
    }

    fn answer(&self) -> String {
        self.data[1..].iter().map(|x| *x.last().unwrap()).collect::<String>()
    }
}

fn doit(fname: &str, part2: bool) {
    let content = fs::read_to_string(fname).expect("can't read file");
    let mut tableau = Tableau::new();


    for line in content.lines() {
        // tableau
        if line.starts_with("move") {
            let parts: Vec<&str> = line.split(' ').collect();
            let count: usize = parts[1].parse().expect("number");
            let from: usize = parts[3].parse().expect("number");
            let to: usize = parts[5].parse().expect("number");
            // println!("{count} {from} {to}");
            tableau.change(count, from, to, part2);
        } else if line == "" {
            tableau.reverse();
        } else if line.contains('[') {
            for (ix,ch) in line.chars().enumerate() {
                if ix % 4 == 1 && ch != ' ' { tableau.push(1+ix/4, ch ) }
            }
        }
    }
    // println!("{:?}", tableau)
    let answer = tableau.answer();
    println!("{fname} {part2} {answer}")
}

fn main() {
    doit("eg.txt", false);
    doit("eg.txt", true);
    doit("input.txt", false);
    doit("input.txt", true);
}
