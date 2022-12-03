use std::fs;

fn bit(c: u8) -> u64 {
    if c > 96 {
        return 1 << (c - 97);
    } else {
        return 1 << (c - 65 + 26);
    }
}

fn bits(sack: &str) -> u64 {
    let mut rval = 0;
    for c in sack.bytes() {
        rval |= bit(c);
    }
    return rval;
}

fn doit(fname: &str) {
    // content needs to be let, or lines() blows up. Maybe because it's an iterator and needs content to stick around
    let content = fs::read_to_string(fname).expect("to read file");
    let mut total: u32 = 0;
    let mut group: u64 = 0;
    let mut total2 = 0;
    let mut i = 0;
    for line in content.lines() {
        let l = line.len() >> 1;
        let flags = bits(&line[0..l]) & bits(&line[l..]);
        total += flags.trailing_zeros() + 1;
        if i % 3 == 0 { group = !0 }
        group &= bits(line);
        if i % 3 == 2 { total2 += group.trailing_zeros() + 1 }
        i += 1;
    }
    println!("total {fname} {total} {total2}")
}

fn main() {
    doit("eg.txt");
    doit("input.txt");
}
