use std::{fs, collections::HashMap};

fn doit(fname: &str) {
    let content = fs::read_to_string(fname).expect("read file");
    let mut path: Vec<&str> = Vec::new();
    let mut sizes: HashMap<String, i32> = HashMap::new();

    for line in content.lines() {
        let parts: Vec<&str> = line.split(' ').collect();
        match parts[..] {
            ["$","cd", "/"] => { path = Vec::new(); } ,
            ["$", "cd", ".."] => { path.pop(); },
            ["$", "cd", name] => { path.push(name); },
            ["$", "ls"] => {},
            ["dir", _] => {},
            [size, _] => {
                let size = size.parse::<i32>().unwrap();
                for i in 0..path.len()+1 {
                    let key = path[0..i].join("/");
                    sizes.entry(key.clone()).or_insert(0);
                    sizes.insert(key.clone(), sizes.get(&key).unwrap() + size);
                }
            },
            _ => {}
        }
    }
    let mut size_list: Vec<(&String, &i32)> = sizes.iter().collect();
    size_list.sort_by(|(_,b), (_,d)| b.cmp(d));
    let (_,used) = size_list.last().unwrap();
    let need = **used - 40000000;

    let mut total = 0;
    let mut part2 = 0;
    for (_,size) in &size_list {
        if **size <= 100000 { total += **size }
        if **size >= need {
            part2 = **size;
            break
        }
    }
    println!("{fname} part1 {total} part2 {part2}");
}

fn main() {
    doit("eg.txt");
    doit("input.txt");
}
