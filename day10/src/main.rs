use std::fs;

enum Instr {
    Noop,
    Addx(i32),
}

fn doit(fname: &str) {
    let content = fs::read_to_string(fname).expect("read file");
    let mut instrs: Vec<Instr> = Vec::new();
    for line in content.lines() {
        let parts: Vec<&str> = line.split(' ').collect();
        match parts[..] {
            ["noop"] => instrs.push(Instr::Noop),
            ["addx", v] => {
                instrs.push(Instr::Noop);
                instrs.push(Instr::Addx(v.parse().unwrap()));

            },
            _ => {}
        }
    }
    
    let mut total = 0;
    let mut regx: i32 = 1;
    let mut result: String = String::new();
    for (ix, ins) in instrs.iter().enumerate() {
        let cyc = (ix + 1) as i32;

        let beam = (cyc - 1) % 40;
        if beam >= regx - 1 && beam <= regx + 1 {
            result.push('#')
        } else {
            result.push(' ');
        }
        if beam == 39 { result.push('\n') }

        if (ix + 1) % 40 == 20 { total += cyc * regx }
        if let Instr::Addx(v) = ins { regx += v; }
    }
    println!("{fname} part1 {total}");
    println!("part2");
    print!("{result}");
}

fn main() {
    doit("eg.txt");
    doit("eg2.txt");
    doit("input.txt");
}
