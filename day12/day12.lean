import Lean
open Std
open Lean

-- TODO figure out how to tell lean the sizes of the arrays
-- match width / height
structure Grid (α : Type) where
  width : Nat
  height : Nat
  data : Array (Array α)
deriving Repr

abbrev Point := (Nat × Nat)

def Grid.find? [BEq α] (needle : α) (grid : Grid α) : Option Point :=
  let rec loop : List (Array α) -> Nat -> Option Point
  | [], _ => none
  | (a :: as), r => match a.getIdx? needle with
      | none => loop as (r + 1)
      | some c => some (r,c)
  loop grid.data.toList 0

def Grid.get! [Inhabited α] (grid : Grid α) : (pt : Point) -> α
| (r,c) => grid.data[r]![c]!

-- What is a more idiomatic solution for this?
-- I've mapped and filtered before, but Nat doesn't go negative,
def Grid.neighbors (grid : Grid α) (pt : Point) : List Point := Id.run do
  let (r,c) := pt
  let mut rval : List Point := []
  if r > 0 then rval := (r-1,c) :: rval
  if c > 0 then rval := (r,c-1) :: rval
  if r+1 < grid.height then rval := (r+1, c) :: rval
  if c+1 < grid.width then rval := (r,c+1) :: rval
  rval

def readGrid (content : String) (f : Char -> α) : Grid α :=
  let lines := ((content.split (· = '\n')).filter fun x => x != "").toArray
  let data := lines.map (fun l => (l.data.map f).toArray)
  let width := data[0]!.size
  let height := data.size
  { width, height, data }
 
abbrev Item := (Nat × Point)

def elev : Nat -> Nat
      | 83 => 97
      | 69 => 122
      | a => a

partial
def step (grid : Grid Nat) (start : Point) (up : Bool) (goal : Nat) : Nat :=
  let rec loop (q : Queue Item) (seen : HashSet Point): Nat :=
    match q.dequeue? with
    | none => 0
    | some ((cnt, pt), q) =>      
      let h  := grid.get! pt
      let canClimb := fun pt => 
        let x := elev (grid.get! pt)
        let h := elev h
        if seen.contains pt then false 
        else if up then x <= h + 1
        else h ≤ x + 1
        
      if seen.contains pt then loop q seen
      else if h == goal then 
        cnt
      else
        let next := ((grid.neighbors pt).filter (canClimb)).map (cnt+1 , · )
        loop (q.enqueueAll next) (seen.insert pt)

  let q := Queue.empty.enqueue (0,start)
  loop q .empty

def main (argv : List String) : IO Unit := do
  let fname := argv[0]!
  println! "{fname}"
  let content <- IO.FS.readFile fname
  let grid := readGrid content (Char.toNat)
  let .some start := (grid.find? 83) | println! "Start not found"
  let .some last  := (grid.find? 69) | println! "End not found"
  println! "{fname} start {start} end {last}"
  
  let part1 := step grid start true 69
  let part2 := step grid last false 97
  println! "{fname} part1 {part1} part2 {part2}"

 #eval main ["day12/eg.txt"]
 #eval main ["day12/input.txt"]
