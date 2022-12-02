use std::fs;

fn main() {

    run("eg.txt");
    run("input.txt");
}

fn run(fname: &str) {
    let contents = fs::read_to_string(fname).expect("can't read {fname}");
    
    let mut total = 0;
    let mut total2 = 0;
    for line in contents.lines() {
        let mut foo = line.bytes();
        let a = i32::from(foo.nth(0).unwrap()) - 65;
        let b = i32::from(foo.nth(1).unwrap()) - 88;
        let score = b + 1 + 3 * ((4 + b - a) % 3);
        total += score;

        let c = (a + (b+2)) % 3;
        let score2 = 3 * b + c + 1;
        total2 += score2;
    }
    println!("{fname} {total} {total2}");
}

