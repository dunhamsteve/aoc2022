import Lean

-- sensor / beacon points

abbrev Point := Int × Int
abbrev Range := Int × Int
abbrev Sensor := Point × Point

-- Is this a hack?
def parseLine (line : String): Point × Point :=
  match ((line.split (λ c => not c.isDigit && c != '-')).filter (·!= "")).map String.toInt! with
  | [a,b,c,d] => ((a,b),(c,d))
  | _ => unreachable!

def Int.abs (a : Int) := if a < 0 then -a else a
-- This has parsing issues
-- notation "‖ " a " ‖" => Int.abs a

def dist : Point -> Point -> Int
| (a,b), (c,d) => (a - c).abs + (b - d).abs

def union : List Range -> Range -> List Range
| [], r => [r]
| (x :: xs), r =>
  if r.1 < x.1 then
    if r.2 + 1 < x.1 then r :: x :: xs
    else union xs (min r.1 x.1, max r.2 x.2)
  else if r.1 > x.2 + 1 then x :: union xs r
  else union xs (min r.1 x.1, max r.2 x.2)

def intersect : List Range -> Range -> List Range
| [], _ => []
| (x :: xs), r =>
  if x.1 < r.1 then
    if x.2 < r.1 then intersect xs r
    else (r.1, min r.2 x.2) :: intersect xs r
  else if x.1 > r.2 then []
  else (max r.1 x.1, min r.2 x.2) :: intersect xs r

def check (sensors : List Sensor) (row : Int)  :=
  let rec loop : List Sensor -> List Range -> List Range
  | [], rs => rs
  | ((a,b) :: ss), rs => 
    let d := dist a b
    let rest := d -  (a.2 - row).abs
    if rest >= 0
      then 
        -- dbg_trace ("add",(a.1-rest,a.1+rest),a,b,dist a b)
        loop ss (union rs (a.1 - rest, a.1 + rest))
      else 
        -- dbg_trace ("skip",a,b,dist a b)
        loop ss rs
  let rs := loop sensors []
  rs

def exclude (size : Int) (ranges : List Range) (s : Sensor) :=
  let d := dist s.1 s.2
  let a := (s.1.2 - size).abs 
  let b := (s.1.2 - 0).abs
  let extra := d - max a b
  if extra >= 0
    then union ranges (s.1.2 - extra, s.1.2 + extra)
    else ranges

def main (argv : List String) : IO Unit := do
  let fname := argv[0]!
  let row := argv[1]!.toInt!
  let size := argv[2]!.toNat!
  let content <- IO.FS.readFile fname
  let sensors := ((content.split (· == '\n')).filter (· != "")).map parseLine
  
  let ranges := check sensors row
  let covered : Int := (ranges.map (λ x => x.2 - x.1 + 1)).foldl (· + · ) 0
  let beacons := (sensors.filter (λ x => x.2.2 == row)).map (λ x => x.2)
  let part1 := (covered - beacons.eraseDups.length)
  println! "{fname} part1 {part1}"
  
  -- This is empty, we may need a better skip heuristic
  let excl := sensors.foldl (exclude size) []
  println! "excl {excl}"
  
  for row in [0:size] do
    let x := check sensors row
    let foo := intersect x (0,size)
    if foo.length != 1 then
      let part2 := ((foo.head!).2+1) * 4000000 + row
      println! "{fname} part2 {part2}"  
      break

#eval main ["day15/eg.txt", "10", "20"]
-- This takes 30s compiled, so I'm deferring it to the built application
-- #eval main ["day15/input.txt","2000000", "4000000"]
