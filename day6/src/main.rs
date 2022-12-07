use std::fs;

fn doit(fname: &str, count: usize) {
    let content = fs:: read_to_string(fname)
        .expect("can't read file");
    for ix in 0 .. content.len()-count {
        let mut tmp : Vec<char> = content[ix..ix+count].chars().collect();
        tmp.sort();
        let mut dup = false;
        for i in 0 .. tmp.len()-1 {
            if tmp[i] == tmp[i+1] { dup = true }
        }
        if !dup {
            println!("{fname} {count} {}", ix + count);
            return
        }
    }
}

fn main() {
    doit("eg.txt",4);
    doit("eg.txt", 14);
    doit("input.txt",4);
    doit("input.txt", 14);
}
