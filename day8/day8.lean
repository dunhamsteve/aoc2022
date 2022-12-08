
import Lean
import Lean.Data.PersistentHashMap
open Lean

structure Forest where
  -- TODO - replace this with an array, lookup should be faster.
  heights : PersistentHashMap (Nat × Nat) Nat
  visible : PersistentHashMap (Nat × Nat) Bool
  width : Nat
  height : Nat

abbrev M (α : Type) := EStateM String Forest α

def parseRow (row : Nat) (line : String) :=
  line.toList.enum.map (fun (col, c) => ((row, col), c.toNat - 48))

def parseFile (content : String) : Forest :=
  let lines := (content.splitOn "\n").filter fun x => x.length > 0
  let rawData := (lines.enum.map (fun (row , line) => parseRow row line)).join
  let heights := rawData.foldl (fun m (a, b) => m.insert a b) .empty
  let height := lines.length
  let width := lines.head!.length
  { heights, height, width, visible := .empty }

def mark (point : Nat × Nat) : M Unit := do
  let st <- get
  set { st with visible := st.visible.insert point true}  

inductive Direction where
| Down : Direction
| Up : Direction
| Left : Direction 
| Right : Direction
deriving Repr  

def Forest.next (f : Forest) :  (dir : Direction) ->  (Nat × Nat) -> Option (Nat × Nat)
| .Down, (r,c) => if r + 1 < f.height then some (r+1,c) else none
| .Up,   (r,c) => if r > 0 then some (r-1,c) else none
| .Left, (r,c) => if c > 0 then some (r,c-1) else none
| .Right,(r,c) => if c + 1 < f.width then some (r,c+1) else none

partial
def scan (row col : Nat) (height : Int) (dir : Direction) : M Unit := do
  let st <- get
  if st.heights[(row,col)].isNone then dbg_trace "none {(row,col)} {(st.height, st.width)}"

  let x := st.heights[(row,col)].get!
  
  if x > height then mark (row,col)
  let x' : Int := if x > height then x else height

  match st.next dir (row,col) with 
  | some (r,c) => scan r c x' dir
  | none => pure ()


def process : M Unit := do
  let f <- get
  for col in [0:f.width] do
    scan 0              col         (-1) .Down
    scan (f.height - 1) col         (-1) .Up
  for row in [0:f.height] do
    scan row          0             (-1) .Right
    scan row          (f.width - 1) (-1) .Left

-- Part 2

partial
def Forest.look (dir: Direction) (r c: Nat) (f : Forest) : Nat :=
  let heights := f.heights
  let max := heights[(r,c)].get!
  let rec loop : Option (Nat × Nat) -> Nat -> Nat
  | none, acc => acc
  | some (r,c), acc =>
      let h := heights[(r,c)].get!
      if h >= max then acc + 1
      else loop (f.next dir (r,c)) (acc + 1)
  loop (f.next dir (r,c)) 0
      
def Forest.score (f : Forest) (r c : Nat) : Nat :=
  (f.look .Down r c) * (f.look .Up r c) * (f.look .Right r c) * (f.look .Left r c)

def part2 (st : Forest) : Nat := Id.run do
  let mut max := 0
  for c in [1:st.width-1] do
    for r in [1:st.height-1] do
      let s := st.score r c
      if s > max then max := s
  max

def main(args: List String) : IO Unit := do
  let fname := args.head!
  let content <- IO.FS.readFile fname

  let mut state := parseFile content
  match process.run state with
  | .ok _ foo => println! "{fname} part1 {foo.visible.toList.length}" 
  | .error msg _ => IO.println <| msg
  -- part2
  println! "{fname} part2 {part2 state}"

#eval main ["day8/eg.txt"]
#eval main ["day8/input.txt"]