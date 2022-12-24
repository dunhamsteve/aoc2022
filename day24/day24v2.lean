import Lean
open Std
open Lean

abbrev Point := (Nat × Nat)
abbrev Data := Array Char
def comparePoint (a b : Point) : Ordering :=
  let _ := @lexOrd; compare a b

abbrev Elves := RBTree Point comparePoint

structure Grid where
  data : Data
  width : Nat
  height : Nat
  start : Point
  finish : Point
  time : Nat
  elves : Elves
deriving Repr

def Grid.get! (g : Grid) (pt : Point) :=
  let ix := (pt.1 + 1) * (g.width + 3) + pt.2 + 1
  g.data[ix]!

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

def Grid.parse (content : String) : Option Grid := do
  let data := content.toList.toArray
  let x : Nat <- data.indexOf? '\n'
  let width := x - 2 -- dot width
  let height := data.size / (width + 3) - 2
  .some { width, height, data, start := (0,0), finish := (height-1,width-1), time := 0, elves := .empty }

def Grid.peek (grid : Grid) (pt : Point) (time : Nat) : Char :=
  let (r,c) := pt
  let row_time := time % grid.height
  let col_time := time % grid.width
  if grid.get! ((grid.height + r - row_time) % grid.height,c) == 'v' then 'v' else
  if grid.get! ((r + row_time) % grid.height,c) == '^' then '^' else
  
  if grid.get! (r, (grid.width + c - col_time) % grid.width) == '>' then '>' else
  if grid.get! (r, (c + col_time) % grid.width) == '<' then '<' else '.'

def Grid.dumpAt (grid : Grid) (time : Nat) : IO Unit := do
  println! "{grid.width} x {grid.height}"
  for r in [0:grid.height] do
    for c in [0:grid.width] do
      IO.print $ grid.peek (r,c) time
    IO.println ""


def Grid.step (grid : Grid) : IO Grid := do
  let width := grid.width
  let height := grid.height
  let time := grid.time + 1

  let mut elves := grid.elves
  let update (elves : Elves) (pt : Point) :=
    if grid.peek pt time == '.' then elves.insert pt else elves
  for (r,c) in elves.toList do
    if grid.peek (r,c) time != '.' then elves := elves.erase (r,c)
    if r + 1 < height then elves := update elves (((r+1) % height), c)
    if r > 0 then elves := update elves (((height + r-1) % height), c)
    if c > 0 then elves := update elves (r, ((width + c-1)% width))
    if c + 1 < width then elves := update elves (r, ((c+1) % width))

  if grid.peek grid.start time == '.' then elves := elves.insert grid.start
  pure { grid with elves, time }

partial
def Grid.run (grid : Grid) : IO Grid := do
  let rec loop (grid : Grid) := do
    if grid.elves.contains grid.finish then return grid
    else loop (<- grid.step)
  loop grid
  
def main (argv : List String) : IO Unit := do
  let fname := argv[0]!
  println! "{fname}"
  let content <- IO.FS.readFile fname
  let .some grid := Grid.parse content
    | println! "failure to parse {fname}"

  let mut grid := grid
  println! "finish {grid.finish}"
  grid <- grid.run
  println! "{fname} part1 {grid.time+ 1}"
  grid := {grid with elves := .empty, start := grid.finish, finish := grid.start }
  grid <- grid.run
  println! "{fname} back {grid.time + 1}"
  grid := {grid with elves := .empty, start := grid.finish, finish := grid.start }
  grid <- grid.run
  println! "{fname} part2 {grid.time + 1}"

--  #eval main ["day24/eg.txt"]
 #eval main ["day24/eg2.txt"]
 #eval main ["day24/input.txt"]
