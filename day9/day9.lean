import Lean

open Lean

structure Grid where
  visited : PersistentHashMap (Int × Int) Unit
  knots : List (Int × Int)

abbrev Point := (Int × Int)

inductive Dir where
| Up : Dir
| Down : Dir
| Left : Dir
| Right : Dir
deriving Repr

instance : Inhabited Dir where
  default := .Up

abbrev Cmd := (Dir × Nat)

def parseLine (line : String) : (Dir × Nat) :=
  match line.splitOn " " with
  | ["U", num] => (.Up, num.toNat!)
  | ["D", num] => (.Down, num.toNat!)
  | ["L", num] => (.Left, num.toNat!)
  | ["R", num] => (.Right, num.toNat!)
  | _ => unreachable!

open Nat

def touch : Int -> Int -> Bool
| a, b => let δ := a - b ; δ <= 1 && (-1) <= δ

def adjust (a b : Int) : Int :=
match compare a b with
| .lt=> a + 1
| .gt => a - 1
| .eq => a

def pull : Point -> Point -> Point
| (x,y), (a,b) =>
  let a' := adjust a x
  let b' := adjust b y
  if (a',b') == (x,y) then (a,b) else (a',b')

def pullAll : Point -> List Point -> List Point
| pt, [] => [pt]
| pt, p::ps => pt :: pullAll (pull pt p) ps

def step (g : Grid) (dir : Dir) : Grid :=
match g.knots with
| [] => g
| (x,y)::rest =>
  let head := match dir with
    | .Up => (x,y+1)
    | .Down => (x,y-1)
    | .Left => (x-1,y)
    | .Right => (x+1,y)
  
  let knots := pullAll head rest
  let tail := knots.getLast!
  {g with knots, visited := g.visited.insert tail () }
    

def update (g : Grid) (cmd : Cmd) : Grid :=
  -- dbg_trace (repr cmd)
  let (dir, n) := cmd
  let rec loop : Grid -> Nat -> Grid
  | g, zero => g
  | g, succ k => loop (step g dir) k
  loop g n

def main(args: List String) : IO Unit := do
  let fname := args.head!
  let content <- IO.FS.readFile fname
  let lines := (content.splitOn "\n").filter fun x => x.length > 0
  let data := lines.map parseLine
  let knots := [(0,0),(0,0)]
  let result := data.foldl update {visited := .empty, knots }
  println! "{fname} part1 {result.visited.toList.length}"
  let knots := .replicate 10 (0,0)
  let result := data.foldl update {visited := .empty, knots }
  println! "{fname} part2 {result.visited.toList.length}"
  

#eval main ["day9/eg.txt"]
#eval main ["day9/input.txt"]