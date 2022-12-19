use std::fs;

type Point = (i32, i32);
type Range = (i32, i32);
type Sensor = (Point, Point);

fn parse_line(line: &str) -> Sensor {
    let parts: Vec<i32> = line
        .split(|c: char| !c.is_numeric() && c != '-')
        .filter(|s| *s != "")
        .map(|s| s.parse().unwrap())
        .collect();
    ((parts[0], parts[1]), (parts[2], parts[3]))
}

// I think this was clearer to me in the lean notation with ::
fn union(rs: &Vec<Range>, r: Range) -> Vec<Range> {
    let mut r = r;
    let mut rval = Vec::new();
    let mut it = rs.iter();
    while let Some(x) = it.next() {
        if r.0 < x.0 {
            if r.1 + 1 < x.0 {
                rval.push(r);
                rval.push(*x);
                rval.extend(it);
                return rval;
            } else {
                r = (r.0.min(x.0), r.1.max(x.1))
            }
        } else if r.0 > x.1 + 1 {
            rval.push(*x);
        } else {
            r = (r.0.min(x.0), r.1.max(x.1))
        }
    }
    rval.push(r);
    rval
}

fn dist(a: &Point, b: &Point) -> i32 {
    (a.0 - b.0).abs() + (a.1 - b.1).abs()
}

fn intersect(rs: &Vec<Range>, r: Range) -> Vec<Range> {
    let mut rval = Vec::new();
    let mut it = rs.iter();
    while let Some(x) = it.next() {
        if x.0 < r.0 {
            if x.1 < r.0 {
                continue;
            } else {
                rval.push((r.0, r.1.min(x.1)))
            }
        } else if x.0 > r.1 {
            return rval;
        } else {
            rval.push((r.0.max(x.0), r.1.min(x.1)));
        }
    }
    rval
}

fn check(sensors: &Vec<Sensor>, row: i32) -> Vec<Range> {
    let mut rval: Vec<Range> = Vec::new();
    for (a, b) in sensors {
        let d = dist(a, b);
        let rest = d - (a.1 - row).abs();
        if rest >= 0 {
            // XXX this is where union happens
            rval = union(&mut rval, (a.0 - rest, a.0 + rest));
        }
    }
    rval
}

fn doit(fname: &str, row: i32, size: i32) {
    let content = fs::read_to_string(fname).unwrap();
    let sensors: Vec<_> = content.lines().map(parse_line).collect();
    let ranges = check(&sensors, row);
    let mut covered = 0;
    for (a, b) in &ranges {
        covered += b - a + 1
    }

    let mut beacons: Vec<_> = sensors.iter().map(|s| s.1).filter(|b| b.1 == row).collect();
    beacons.sort();
    beacons.dedup();
    covered = covered - beacons.len() as i32;
    // unique?

    println!("{fname} part1 {covered}");
    for row in 0..size {
        let x = check(&sensors, row);
        let foo = intersect(&x, (0, size));
        if foo.len() != 1 {
            let col: i64 = (foo[0].1 + 1).into();
            let row: i64 = row.into();
            let size: i64 = size.into();
            let part2: i64 = col * size + row;
            println!("{fname} part2 {part2}");
        }
    }
}

fn main() {
    doit("eg.txt", 10, 20);
    doit("input.txt", 2000000, 4000000);
}
