import Lean
open Std
open Lean

abbrev Point := (Nat × Nat)
abbrev Data := Array UInt32

structure Grid where
  data : Data
  width : Nat
  height : Nat
  start : Nat
  finish : Nat
  time : Nat
deriving Repr

def Grid.get! (g : Grid) (pt : Point) :=
  let ix := pt.1 * g.width + pt.2
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
  let chars := content.toList.toArray
  let x : Nat <- chars.indexOf? '\n'
  let width := x - 2 -- dot width
  let height := chars.size / (width + 3) - 2
  let mut data := mkArray (width * height) 0
  for r in [0:height] do
    for c in [0:width] do
      let ix := (r + 1) * (width + 3) + c + 1
      data := data.insertAt! (r*width + c) (toMask (chars[ix]!))
  .some { width, height, data := data, start := 0, finish := width * height - 1, time := 1 }

def Grid.dump (grid : Grid) : IO Unit := do
  println! "{grid.width} x {grid.height}"
  for r in [0:grid.height] do
    for c in [0:grid.width] do
      IO.print (fromMask $ grid.data[r*grid.width + c]!)
    IO.println ""

def Grid.step (grid : Grid) : IO Grid := do
  let start := grid.start
  let width := grid.width
  let height := grid.height
  let mut data : Array UInt32 := Array.mkArray (width*height) 0
  let update (data: Array UInt32) (r c : Nat) (m : UInt32) := 
    let ix := 
      r * width + c
    data.set! ix (m ||| data[ix]!)
  
  for r in [0:height] do
    for c in [0:width] do
      let ch := grid.data[r*width+c]!
      if ch &&& 1 != 0 then do
          data := update data r ((c + 1) % width) 1
      if ch &&& 4 != 0 then data := update data r ((width + c - 1) % width) 4
      if ch &&& 2 != 0 then data := update data ((r + 1 ) % height) c 2
      if ch &&& 8 != 0 then data := update data ((height + r - 1) % height) c 8
  
  for r in [0:height] do
    for c in [0:width] do
      if 16 == grid.data[r*width + c]! then
        data := update data r c 16
        if r + 1 < height then data := update data ((r+1) % height) c 16
        if r > 0 then data := update data ((height + r-1) % height) c 16
        if c > 0 then data := update data r ((width + c-1)% width) 16
        if c + 1 < width then data := update data r ((c+1) % width) 16
  
  if data[start]! == 0 then data := data.set! start 16
  pure {grid with data, time := grid.time + 1}

partial
def part1 (grid : Grid) : IO Grid := do
  let ix := grid.finish
  let rec loop (grid : Grid) := do
    if grid.data[ix]! == 16 then return grid
    else loop (<- grid.step)
  loop grid
  
def main (argv : List String) : IO Unit := do
  let fname := argv[0]!
  println! "{fname}"
  let content <- IO.FS.readFile fname
  let .some grid := Grid.parse content
    | println! "failure to parse {fname}"
  let mut grid := grid
  grid <- part1 grid
  grid := {grid with data := grid.data.map λ c => c &&& 15, start := grid.finish, finish := grid.start }
  println! "{fname} part1 {grid.time}"
  grid <- part1 grid
  println! "{fname} back {grid.time}"
  grid := {grid with data := grid.data.map λ c => c &&& 15, start := grid.finish, finish := grid.start }
  grid <- part1 grid
  println! "{fname} part2 {grid.time}"

--  #eval main ["day24/eg.txt"]
 #eval main ["day24/eg2.txt"]
 #eval main ["day24/input.txt"]
