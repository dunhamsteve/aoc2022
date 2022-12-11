import Lean
open Lean

structure Forest where
  heights : Array (Array Nat)
  width : Nat
  height : Nat

def parseFile (content : String) : Forest :=
  let lines := (content.splitOn "\n").filter fun x => x.length > 0
  let parseRow := fun line => (line.toList.map (fun c => (c.toNat - 48))).toArray
  let heights := (lines.map parseRow).toArray
  let height := lines.length
  let width := lines.head!.length
  { heights, height, width }

inductive Direction where
| Down : Direction
| Up : Direction
| Left : Direction 
| Right : Direction
deriving Repr  

inductive LazyList (α : Type)
| nil : LazyList α
| cons : α -> Thunk (LazyList α) -> LazyList α

instance : Inhabited (LazyList α) := ⟨ .nil ⟩ 

partial
def Forest.view (dir : Direction) (r c : Nat) (f : Forest) : LazyList Nat :=
  match dir with
  | .Up    => if r > 0 then .cons f.heights[r-1]![c]! (f.view dir (r-1) c) else .nil
  | .Down  => if r+1 < f.height then .cons f.heights[r+1]![c]! (f.view dir (r + 1) c) else .nil
  | .Left  => if c > 0 then .cons f.heights[r]![c-1]! (f.view dir r (c-1)) else .nil
  | .Right => if c+1 < f.width then .cons f.heights[r]![c+1]! (f.view dir r (c+1)) else .nil

def Forest.look (dir : Direction) (r c : Nat) (f : Forest) : Nat :=
  let me := f.heights[r]![c]!
  let rec loop : LazyList Nat -> Nat -> Nat
  | .nil, acc => acc
  | (.cons h rest), acc => if h >= me then acc + 1 else loop rest.get (acc + 1)
  loop (f.view dir r c) 0
  
def Forest.visibleDir (f : Forest) (dir : Direction) (r c : Nat) :=
  let me := f.heights[r]![c]!
  let rec loop : LazyList Nat -> Bool
  | .nil => true
  | (.cons h rest) => if h < me then loop rest.get else false
  loop (f.view dir r c)
  
def Forest.visible (f : Forest) (r c : Nat) :=
  f.visibleDir .Up r c || f.visibleDir .Down r c || f.visibleDir .Left r c || f.visibleDir .Right r c

def Forest.score (f : Forest) (r c : Nat) : Nat :=
  (f.look .Down r c) * (f.look .Up r c) * (f.look .Right r c) * (f.look .Left r c)

def main(args: List String) : IO Unit := do
  let fname := args.head!
  let content <- IO.FS.readFile fname

  let mut f := parseFile content
  let mut max := 0
  let mut vis := 0
  for c in [0:f.width] do
    for r in [0:f.height] do
      let s := f.score r c
      if s > max then max := s
      if f.visible r c then vis := vis + 1
  
  println! "{fname} part1 {vis} part2 {max}"
  
#eval main ["day8/eg.txt"]
#eval main ["day8/input.txt"]