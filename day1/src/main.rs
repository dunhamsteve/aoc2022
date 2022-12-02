use std::env;
use std::fs;

// I assumed the part two would involve the individual sizes, it did not.
struct Elf {
    total : i32,
    sizes: Vec<i32>
}

fn main() {
    let args: Vec<String> = env::args().collect();
    let mut elves: Vec<Elf> = Vec::new();
    
    let file_path = &args[1];

    println!("File {}", file_path);

    let contents = fs::read_to_string(file_path).expect("to read file");
    let lines = contents.lines();
    let mut elf: Elf = Elf { total: 0, sizes: Vec::new()};
    for line in lines {
        println!("{}", line.len());
        if line.len() > 0 {
            let v : i32 = line.parse().unwrap();
            elf.sizes.push(v);
            elf.total += v;
        } else {
            elves.push(elf);
            elf = Elf { total: 0, sizes: Vec::new()}
        }
    }
    // Part 1: O(n)
    let mut max: i32 = 0;
    for elf in &elves {
        println!("{}", elf.total);
        if elf.total > max { max = elf.total };
    }
    println!("max {max}");

    // Part 2: could be O(n), but O(n log n) was less typing
    elves.sort_by(|a, b| a.total.cmp(&b.total) );
    elves.reverse();
    let a = elves[0].total;
    let b = elves[1].total;
    let c = elves[2].total;
    let total = a + b + c;
    println!("total {a}+{b}+{c} = {total}");

}
