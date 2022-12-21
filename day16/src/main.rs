use rb_tree::RBTree;
use std::fs;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
struct Node {
    name: String,
    index: u64,
    rate: u64,
    out: u64,
}

impl Node {
    fn mask(&self) -> u64 {
        1 << self.index
    }
}

fn get_num(s: &str) -> u64 {
    let i = s.find(|c| c <= '9');
    s.trim_start_matches(|c : char| !c.is_numeric()).trim_end_matches(|c : char| !c.is_numeric() ).parse().unwrap()
}

fn parse_file(content: &str) -> Vec<Node> {
    let lines: Vec<Vec<&str>> = content.lines().map(|l| l.split(' ').collect()).collect();
    let names: Vec<&str> = lines.iter().map(|pts| pts[1]).collect();
    let get_index = |s: &str| names.iter().enumerate().find(|&r| *r.1 == s).unwrap().0;
    let mut rval = Vec::new();
    for pts in lines {
        let name = pts[1];
        let index = get_index(name);
        let rate: u64 = get_num(pts[4]);
        let mut out = 0;
        for p in &pts[9..] {
            let ix = get_index(p.trim_end_matches(','));
            out = out | (1 << ix);
        }
        rval.push(Node {
            name: name.to_string(),
            index: index as u64,
            rate,
            out,
        });
    }
    rval
}

type Queue = RBTree<State>;
type Nodes = Vec<Node>;

struct Data {
    nodes: Nodes,
    skip: u64,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
struct State {
    est: u64,
    score: u64,
    my_time: u64,
    el_time: u64,
    closed: u64,
    my_node: Node,
    el_node: Node,
}

impl State {
    fn is_closed(&self, ix: u64) -> bool {
        self.closed & (1 << ix) != 0
    }
    fn open_valve(&self) -> Option<State> {
        if self.my_time > 0 && self.is_closed(self.my_node.index) {
            let mut rval = self.clone();
            rval.closed = rval.closed ^ self.my_node.mask();
            rval.my_time -= 1;
            rval.score += rval.my_time * self.my_node.rate;
            return Some(rval);
        }
        None
    }
    fn move_to(&self, node: &Node) -> Option<State> {
        if self.my_time > 0 {
            let mut rval = self.clone();
            rval.my_time = rval.my_time - 1;
            rval.my_node = node.clone();
            return Some(rval);
        }
        None
    }
    fn open_el_valve(&self) -> Option<State> {
        if self.el_time > 0 && self.is_closed(self.el_node.index) {
            let mut rval = self.clone();
            rval.closed = rval.closed ^ self.el_node.mask();
            rval.el_time -= 1;
            rval.score += rval.el_time * self.el_node.rate;
            return Some(rval);
        }
        None
    }
    fn move_el_to(&self, node: &Node) -> Option<State> {
        if self.el_time > 0 {
            let mut rval = self.clone();
            rval.el_time = rval.el_time - 1;
            rval.el_node = node.clone();
            return Some(rval);
        }
        None
    }
}

fn estimate(data: &Data, st: &mut State) {
    let mut est = st.score;
    let mut my_time = st.my_time;
    let mut el_time = st.el_time;
    let mut my_skip = 2;
    let mut el_skip = 2;
    
    if st.is_closed(st.my_node.index) {
        est += st.my_node.rate * (my_time - 1);
    }
    if st.is_closed(st.el_node.index) {
        est += st.el_node.rate * (el_time - 1);
    }

    for n in &data.nodes {
        if st.is_closed(n.index) && n.index != st.my_node.index && n.index != st.el_node.index {
            if my_time >= el_time && my_time >= my_skip {                
                my_time -= my_skip;
                est += n.rate * my_time;
                my_skip = data.skip;
            } else if el_time >= el_skip {
                el_time -= el_skip;
                est += n.rate * el_time;
                el_skip = data.skip;
            }
        }
    }
    st.est = est
}

fn process(data: &Data, start: State) -> u64 {
    let mut queue: Queue = RBTree::new();
    let mut best: u64 = 0;
    queue.insert(start);

    loop {
        if let Some(st) = queue.pop_back() {
            if st.est <= best {
                return best
            }
            best = st.score.max(best);
            let mut my_states : Vec<State> = Vec::new();
            for node in &data.nodes {
                if node.mask() & st.my_node.out != 0 {
                    if let Some(st) = st.move_to(&node) {
                        my_states.push(st);
                    }
                }
            }
            if let Some(st) = st.open_valve() {
                my_states.push(st);
            }
            for st in &my_states {
                // short circuit for part1
                if st.el_time == 0 {
                    let mut st = st.clone();
                    estimate(data, &mut st);
                    queue.insert(st);
                    continue
                }
                for node in &data.nodes {
                    if node.mask() & st.el_node.out != 0 {
                        if let Some(mut st) = st.move_el_to(node) {
                            estimate(data, &mut st);
                            queue.insert(st);
                        }
                    }
                }
                if let Some(mut st) = st.open_el_valve() {
                    estimate(data, &mut st);
                    queue.insert(st);   
                }
            }
        } else { 
            return best
        }
    }
}

fn doit(fname: &str) {
    let content = fs::read_to_string(fname).expect("file not found");
    let mut nodes = parse_file(&content);
    // nodes need to be sorted by desc rate
    nodes.sort_by(|a, b| b.rate.cmp(&a.rate));

    let mut closed = 0;
    for node in &nodes {
        if node.rate > 0 { 
            closed = closed | node.mask()
        }
    }

    // build skip value, etc
    let mut skip = 4;
    for n in &nodes {
        if n.rate == 0 { continue }
        for m in &nodes {
            if m.mask() & n.out != 0 && m.rate != 0 {
                skip = 2;
            }
        }
    }

    let data = Data {skip, nodes};
    let node = data.nodes.iter().find(|n| n.name == "AA").expect("to find AA").clone();
    let mut start = State { my_time: 30, el_time: 0, closed, score: 0, est: 0, my_node: node.clone(), el_node: node.clone() };

    estimate(&data, &mut start);
    let part1 = process(&data, start);

    let mut start = State { my_time: 26, el_time: 26, closed, score: 0, est: 0, my_node: node.clone(), el_node: node.clone() };
    estimate(&data, &mut start);
    let part2 = process(&data, start);
    println!("{fname} part1 {part1} part2 {part2}");

}

fn main() {
    doit("eg.txt");
    doit("input.txt");
}
