use std::fs;

type Pile = Vec<u8>;
type Rock = [u8; 4];

#[derive(Clone, Copy, Debug)]
enum Dir {
    Left,
    Right,
}

struct State {
    winds: Vec<Dir>,
    pile: Vec<u8>,
    wstep: usize,
    rstep: usize,
}

impl State {
    fn getWind(&mut self) -> Dir {
        let rval = self.winds[self.wstep % self.winds.len()];
        self.wstep += 1;
        rval
    }
    fn getRock(&mut self) -> Rock {
        let rval = rocks[self.rstep % rocks.len()];
        self.rstep += 1;
        rval.clone()
    }
    fn doWind(&mut self, rock: &Rock, pos: usize) -> Rock {
        let mut rval = rock.clone();
        let wind = self.getWind();
        shift(&mut rval, wind);
        // println!("dowind {:?} {:?}", wind, rval);
        if testRock(&rval, &self.pile, pos) {
            rval
        } else {
            // println!("test fail");
            *rock
        }
    }
}

const rocks: &[&[u8; 4]] = &[
    &[0x1e, 0x0, 0x0, 0x0],
    &[0x8, 0x1c, 0x8, 0x0],
    &[0x1c, 0x4, 0x4, 0x0],
    &[0x10, 0x10, 0x10, 0x10],
    &[0x18, 0x18, 0x0, 0x0],
];

fn shift(rock: &mut Rock, dir: Dir) {
    match dir {
        Dir::Left => {
            if rock.iter().all(|c| c & 64 == 0) {
                for c in rock.iter_mut() {
                    *c = *c << 1
                }
            }
        }
        Dir::Right => {
            if rock.iter().all(|c| c & 1 == 0) {
                for c in rock.iter_mut() {
                    *c = *c >> 1
                }
            }
        }
    }
}

// test rock at end of pile.
fn testRock(rock: &Rock, pile: &Pile, pos: usize) -> bool {
    // just position after the pile, so
    for i in 0..rock.len() {
        if pos + i < pile.len() {
            if pile[pos + i] & rock[i] != 0 {
                return false;
            }
        }
    }
    true
}

fn place(rock: &Rock, pile: &mut Pile, pos: usize) {
    if pos + rock.len() > pile.len() {
        pile.resize(pos + rock.len(), 0);
    }
    for i in 0..rock.len() {
        pile[pos + i] = pile[pos + i] | rock[i]
    }
}

fn drop_rock(st: &mut State) {
    let mut rock = st.getRock();
    let mut pos = st.pile.len() + 3;
    loop {
        rock = st.doWind(&rock, pos);
        if pos == 0 || !testRock(&rock, &st.pile, pos - 1) {
            place(&rock, &mut st.pile, pos);
            while st.pile.last() == Some(&0) {
                st.pile.pop();
            }
            return;
        } else {
            pos -= 1;
        }
    }
}

fn to_dir(c: char) -> Dir {
    match c {
        '<' => Dir::Left,
        '>' => Dir::Right,
        _ => todo!(),
    }
}

fn dump(st: &State) {
    let height = st.pile.len();
    println!(
        "rstep {} wstep {} height {}",
        st.rstep,
        st.wstep,
        st.pile.len()
    );

    for i in 0..height {
        for j in 0..7 {
            if st.pile[height - i - 1] & (1 << (7 - j - 1)) == 0 {
                print!(".");
            } else {
                print!("#");
            }
        }
        println!();
    }
}

fn doit(fname: &str) {
    println!("\n# {fname}");
    let content = fs::read_to_string(fname).expect("to read file");
    let winds: Vec<Dir> = content.trim().chars().map(to_dir).collect();
    let mut st = State {
        winds: winds.clone(),
        pile: Vec::new(),
        wstep: 0,
        rstep: 0,
    };
    for _ in 0..2022 {
        drop_rock(&mut st);
    }

    println!("part1 {}", st.pile.len());
    let mut st = State {
        winds: winds.clone(),
        pile: Vec::new(),
        wstep: 0,
        rstep: 0,
    };
    let mut st2 = State {
        winds: winds.clone(),
        pile: Vec::new(),
        wstep: 0,
        rstep: 0,
    };
    loop {
        drop_rock(&mut st);
        drop_rock(&mut st2);
        drop_rock(&mut st2);
        if st.wstep % st.winds.len() == st2.wstep % st2.winds.len()
            && st.rstep % rocks.len() == st2.rstep % rocks.len()
        {
            let period: u64 = st.rstep as u64;
            let todo = 1000000000000 - period;
            let div = todo / period;
            let rem = todo % period;
            let delta_h = (st2.pile.len() - st.pile.len()) as u64;
            for _ in 0..rem {
                drop_rock(&mut st);
            }
            let height = st.pile.len() as u64;

            let part2 = delta_h * div + height;
            println!("part2 {part2}");
            break;
        }
    }
}
fn main() {
    doit("eg.txt");
    doit("input.txt");
}
