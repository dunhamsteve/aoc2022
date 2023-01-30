import Lean
open Std
open Lean

abbrev Point := (Nat × Nat)
abbrev Data := Array UInt32

/-
I ended up with maxcol / maxrow so I'd know they're > 0. This let me get
start / finish inhabited and makes the .ofNat in step work. 
I have a spike trying to get width/height back in by passing start/finish to 
newGrid, but hit the toNat problems.  Maybe I could throw 0 < width, 0 < height
into the struct, but maxcol/maxrom may be less friction.
-/
structure Grid (maxcol : Nat) (maxrow : Nat) where
  data : Data
  start : Fin maxrow.succ × Fin maxcol.succ
  finish : Fin maxrow.succ × Fin maxcol.succ
  time : Nat
  hsz : data.size = maxrow.succ * maxcol.succ
deriving Repr

def toMask : Char -> UInt32
| '>' => 1
| 'v' => 2
| '<' => 4
| '^' => 8
| _   => 0

def fromMask : UInt32 -> Char
| 1 => '>'
| 2 => 'v'
| 4 => '<'
| 8 => '^'
| 16 => '@'
| 0 => '.'
| _ => '*'

namespace Grid

def newGrid : Grid maxcol maxrow :=
  let data := mkArray (maxrow.succ * maxcol.succ) 0
  { data, start := ⟨0,0⟩ , finish := ⟨ ⟨ maxrow, maxrow.lt_succ_self ⟩ , ⟨maxcol, maxcol.lt_succ_self ⟩ ⟩ ,
    time := 1, hsz := by simp }

def mkpos (grid : Grid maxcol maxrow) (r : Fin maxrow.succ) (c : Fin maxcol.succ): Fin grid.data.size :=
  let pos := r * maxcol.succ + c
  have : pos < grid.data.size := by
    rw [grid.hsz]
    apply Nat.lt_of_lt_of_le
    apply Nat.add_lt_add_left c.2
    rw [<-Nat.succ_mul]
    apply Nat.mul_le_mul_right
    apply Nat.succ_le_of_lt
    exact r.2
  ⟨pos, this⟩

def set (grid : Grid maxcol maxrow) (r : Fin maxrow.succ) (c : Fin maxcol.succ) (val : UInt32): Grid maxcol maxrow :=
  let pos := grid.mkpos r c
  {grid with data := grid.data.set pos val, hsz := by simp [grid.hsz]}

def update (grid : Grid maxcol maxrow) (r : Fin maxrow.succ) (c : Fin maxcol.succ) (val : UInt32): Grid maxcol maxrow :=
  let pos := grid.mkpos r c
  let prev := grid.data.get pos
  {grid with data := grid.data.set pos (val ||| prev), hsz := by simp [grid.hsz]}

def get (grid : Grid maxcol maxrow) (r : Fin maxrow.succ) (c : Fin maxcol.succ): UInt32 :=
  let pos := grid.mkpos r c
  grid.data.get pos

-- I was just using map on data, but I can't prove the size is the same
def reset (grid : Grid maxcol maxrow) : Grid maxcol maxrow := Id.run do
  let mut rval := grid
  for hr : r in [0:maxrow.succ] do 
    let fr := ⟨ r, hr.2⟩ 
    for hc : c in [0:maxcol.succ] do
      let fc := ⟨c, hc.2⟩ 
      rval := rval.set fr fc (15 &&& rval.get fr fc)
  rval

end Grid

-- The original parser was a hack. This is too, but the indices are proven.
def Grid.parse (content : String) : Option (Σ (sz : Nat × Nat), Grid sz.1 sz.2) := do
  let lines := (((content.trim.splitOn "\n").dropLast.drop 1).map λ l => (l.toList.dropLast.drop 1).toArray).toArray
  let height := lines.size
  if h: 0 < height then
    let width := lines[0].size
    let maxcol := width - 1
    let maxrow := height - 1
    let mut data : Grid maxcol maxrow := Grid.newGrid
    for h : r in [0:maxrow.succ], line in lines do
      for h1 : c in [0:maxcol.succ], ch in line do
        data := data.set ⟨ r, h.2 ⟩ ⟨ c, h1.2 ⟩  (toMask ch)
    .some ⟨ (maxcol,maxrow), data⟩
  else .none

def Grid.dump (grid : Grid maxcol maxrow) : IO Unit := do
  println! "{maxcol.succ} x {maxrow.succ}"
  for r in [0:maxrow.succ] do
    for c in [0:maxcol.succ] do
      IO.print (fromMask $ grid.get (.ofNat r) (.ofNat c))
    IO.println ""

def Grid.step (grid : Grid maxcol maxrow) : IO (Grid maxcol maxrow) := do
  let start := grid.start

  let mut data : Grid maxcol maxrow := Grid.newGrid
  let width := maxcol.succ
  let height := maxrow.succ

  -- update wind, this is modular so we .ofNat
  for h : r in [0:height] do
    let fr := ⟨r, h.2⟩  
    for h1 : c in [0:width] do
      let fc := ⟨c, h1.2⟩ 
      
      let ch := grid.get fr fc
      if ch &&& 1 != 0 then data := data.update fr  (.ofNat (c + 1)) 1
      if ch &&& 4 != 0 then data := data.update fr  (.ofNat (width + c - 1)) 4
      if ch &&& 2 != 0 then data := data.update (.ofNat (r + 1 )) fc 2
      if ch &&& 8 != 0 then data := data.update (.ofNat (height + r - 1)) fc  8

  -- update elves, not modular so I'll provide index proofs
  for hr : r in [0:height] do
    let fr := ⟨ r, hr.2 ⟩
    for hc: c in [0:width] do
      let fc := ⟨c, hc.2⟩ 
      if 16 == grid.get fr  fc  then
        data := data.update fr fc 16
        if h : r + 1 < height then data := data.update ⟨r+1, h⟩ fc 16
        if h : c + 1 < width then data := data.update fr ⟨ c + 1, h ⟩ 16
        if 0 < r then
          have : r - 1 < height := by apply Nat.lt_of_le_of_lt _ hr.2;  apply Nat.pred_le
          data := data.update ⟨ r - 1, this ⟩ fc 16
        if 0 < c then
          have : c - 1 < width := by apply Nat.lt_of_le_of_lt _ hc.2; apply Nat.pred_le
          data := data.update fr ⟨ c - 1, this⟩ 16

  if data.get start.1 start.2 == 0 then data := data.set start.1 start.2 16
  pure {data with time := grid.time + 1, start := grid.start, finish := grid.finish}

-- I think there exist inputs that run forever, so this is partial without fuel.
partial
def part1(grid : Grid w h) : IO (Grid w h) := do
  let rec loop (grid : Grid w h) : IO (Grid w h) := do
    let ix := grid.finish
    if grid.get ix.1 ix.2 == 16 then return grid
    else loop (<- grid.step)
  loop grid

def main (argv : List String) : IO Unit := do
  let fname := if h : 0 < argv.length then argv[0] else "eg.txt"
  println! "{fname}"
  let content <- IO.FS.readFile fname
  let .some ⟨ _, grid ⟩  := Grid.parse content
    | println! "failure to parse {fname}"
  -- grid.dump
  let mut grid := grid
  grid <- part1 grid
  grid := grid.reset
  grid := {grid with start := grid.finish, finish := grid.start}
  println! "{fname} part1 {grid.time}"
  grid <- part1 grid
  println! "{fname} back {grid.time}"
  grid := grid.reset
  grid := {grid with start := grid.finish, finish := grid.start}
  grid <- part1 grid
  println! "{fname} part2 {grid.time}"

--  #eval main ["day24/eg.txt"]
 #eval main ["day24/eg2.txt"]
 #eval main ["day24/input.txt"]
