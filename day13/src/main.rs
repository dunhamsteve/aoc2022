use std::{fs, str::FromStr, num::ParseIntError, cmp::Ordering };

#[derive(Debug,Clone,PartialEq)]
enum Expr {
    Cons(Box<Expr>,Box<Expr>),
    Nil,
    Val(u32),
}

struct PState {
    source: Vec<char>,
    pos: usize
}

fn nil() -> Box<Expr> {
    Box::new(Expr::Nil)
}

impl PState {
    fn maybe(&mut self, ch: char) -> bool {
        if self.source[self.pos] == ch {
            self.pos += 1;
            true
        } else {
            false
        }
    }
    fn number(&mut self) -> u32 {
        let mut a = 0;
        loop {
            if let Some(d) = self.source[self.pos].to_digit(10) {
                a = a * 10 + d;
                self.pos += 1;
            } else {
                return a;
            }
        }
    }
    fn parse_expr(&mut self) -> Box<Expr> {
        use Expr::*;
        if self.maybe('[') {
            
            let mut tmp : Vec<Box<Expr>> = Vec::new();
            loop {
                if self.maybe(']') {
                    let mut rval = nil();
                    tmp.reverse();
                    for expr in tmp {
                        rval = Box::new(Cons(expr,rval));
                    }
                    return rval;
                }
                tmp.push(self.parse_expr());
                self.maybe(',');
            }
        } else {
            return Box::new(Val(self.number()));
        }
    }
}

fn parse_line(line: &str) -> Box<Expr> {
    let mut st = PState { 
        source: line.chars().collect(),
        pos: 0
    };
    st.parse_expr()
}

fn compare(e1: &Box<Expr>, e2: &Box<Expr>) -> Ordering {
    match &**e1 {
        Expr::Cons(a, rest) => match &**e2 {
            Expr::Cons(b, rest2) => match compare(&a,&b) {
                
                Ordering::Equal => compare(&rest, &rest2),
                ord => ord
            },
            Expr::Nil => Ordering::Greater,
            Expr::Val(_) => compare(e1, &Box::new(Expr::Cons(e2.clone(), nil()))),
        },
        Expr::Nil => match **e2 {
            Expr::Nil => Ordering::Equal,
            _ => Ordering::Less,
        },
        Expr::Val(a) => match **e2 {
            Expr::Cons(_, _) => compare(&Box::new(Expr::Cons(e1.clone(), nil())),e2),
            Expr::Nil => Ordering::Greater,
            Expr::Val(b) => a.cmp(&b),
        },
    }
}

fn doit(fname: &str) {
    let content = fs::read_to_string(fname).expect("read file");
    let mut exprs : Vec<Box<Expr>> = Vec::new();

    for line in content.lines() {
        if line != "" {
            exprs.push(parse_line(line));
        }
    }
    let mut part1 = 0;
    for ix in (0..exprs.len()).step_by(2) {
        if compare(&exprs[ix],&exprs[ix+1]) == Ordering::Less {
            part1 += ix/2 + 1;
        }
    }

    // insert stuff
    use Expr::*;
    let two = Box::new(Cons(Box::new(Val(2)), nil()));
    let six = Box::new(Cons(Box::new(Val(6)), nil()));
    exprs.push(two.clone());
    exprs.push(six.clone());
    exprs.sort_by(compare);
    let x = exprs.iter().position(|p| *p == two).unwrap();
    let y = exprs.iter().position(|p| *p == six).unwrap();
    let part2 = (x + 1) * (y + 1);
    println!("{fname} part1 {part1} part2 {part2}");

}

fn main() {
    doit("eg.txt");
    doit("input.txt");
}
